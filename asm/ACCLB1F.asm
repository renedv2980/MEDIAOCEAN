*          DATA SET ACCLB1F    AT LEVEL 031 AS OF 03/13/01                      
*PHASE T6211FA                                                                  
CLB1F    TITLE '- BILL PROGRAM UPDATE BILLING'                                  
* DPEA 024 - CODE FOR PC BILLING (FLEXIBILL)                                    
* DPEA 025 - SAVE FOREIGN CURRENCY COMMISSION ON 99 POSTING                     
* DPEA 026 - REMOVE PBVATT / PBVATTF FOR VAT TOTALS                             
*          - TOTAL UP PBTOTFV (INSTEAD OF CALCULATING FROM PBTOTAV)             
CLB1F    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CB1F**,RR=RE                                                 
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING PRORATAD,LSPRATA                                                 
         USING MIXLST,LSMIXLST                                                  
         USING PBLKD,R7                                                         
         USING POSTVALS,PBPOSTV                                                 
         L     RC,AOVERWRK                                                      
         USING BWORKD,RC                                                        
         ST    RE,BORELO                                                        
*                                                                               
         BAS   RE,INI              INITAILIZATION                               
         BNE   EXIT                                                             
*                                                                               
         GOTO1 APRCTRNS            PROCESS TRANSACTIONS                         
         BNE   EXIT                                                             
         GOTO1 AINIPST             INITIALIZE FOR POSTINGS                      
         BNE   EXIT                                                             
         GOTO1 APSTIST             POST INCOME SUSPENSE TRANSFERS               
         BNE   EXIT                                                             
         GOTO1 APSTBIL             POST 99 BILLING                              
         BNE   EXIT                                                             
         GOTO1 APSTINC             POST INCOME                                  
         BNE   EXIT                                                             
         GOTO1 APSTDEB             POST DEBTORS                                 
         BNE   EXIT                                                             
         GOTO1 APSTDSC             POST DISCOUNT/SURCHARGE                      
         BNE   EXIT                                                             
         GOTO1 APSTVAT             POST VAT                                     
         BNE   EXIT                                                             
         GOTO1 APSTPST             POST PST                                     
         BNE   EXIT                                                             
         GOTO1 APSTCST             POST COSTING                                 
         BNE   EXIT                                                             
         GOTO1 APSTWUP             POST WRITE-UPS                               
         BNE   EXIT                                                             
         GOTO1 APSTREP             POST REVERSE ESTIMATED PRODUCTION            
         BNE   EXIT                                                             
         GOTO1 AUPDPRB             READ AND UPDATE 99 PRIOR BILLS               
         BNE   EXIT                                                             
         L     RF,AHDRMNF                                                       
         TM    BCINDS1,BCPCBILL    TEST PC BILLING                              
         BZ    *+8                                                              
         L     RF,AHDRPCB                                                       
         GOTO1 (RF)                UPDATE BILL HEADER RECORD                    
         BNE   EXIT                                                             
*                                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
*              INITIALIZATION                                         *         
***********************************************************************         
         SPACE 1                                                                
INI      NTR1  ,                                                                
         LA    R0,AROUTN           R0=(NUMBER OF ROUTINES)                      
         LA    RF,AREROUT          RF=A(RELOCATED ADDRESSES)                    
         XR    RE,RE                                                            
         LA    R1,ROUT                                                          
INI02    ST    R1,0(RF)                                                         
         STC   RE,0(RF)                                                         
         LA    RE,1(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,INI02                                                         
*                                                                               
         CLI   PBMODE,PBLIVEQ      TEST LIVE                                    
         BNE   INI04                                                            
         TM    PBINDS1,PBIAUTO     TEST BILL NUMBER AUTO SET                    
         BZ    INI04                                                            
         LHI   RE,IOAREA5-WORKD    SAVE IO5 (TO UPDATE IT LATER)                
         LA    RE,WORKD(RE)                                                     
         LHI   RF,IOAREALN                                                      
         LA    R0,BIOAREA5                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
INI04    DS    0H                                                               
*&&US                                                                           
         XC    BSANAC,BSANAC                                                    
         L     R2,GOAPRO-GOBLOCKD(RF)                                           
         LA    R2,ACCORFST(R2)                                                  
         SR    R0,R0                                                            
         USING SANELD,R2                                                        
INI10    CLI   SANEL,0                                                          
         BE    INI20                                                            
         CLI   SANEL,SANELQ                                                     
         BE    *+14                                                             
         IC    R0,SANLN                                                         
         AR    R2,R0                                                            
         B     INI10                                                            
         MVC   BSANAC,SANCODE                                                   
         MVC   BSANNM,SANNAME                                                   
         DROP  R2                                                               
INI20    DS    0H                                                               
*&&                                                                             
         LA    R2,IOKEY            READ PRODUCTION MEDIA RECORD                 
         USING PMDRECD,R2                                                       
         MVC   PMDKEY,BCSPACES                                                  
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,CUABIN                                                   
         XR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         LA    RE,BCJOBCOD(RE)                                                  
         MVC   PMDKMED,0(RE)                                                    
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         LA    R3,PMDRFST                                                       
         USING PMDELD,R3                                                        
         XR    RF,RF                                                            
INI22    CLI   PMDEL,PMDELQ                                                     
         BE    INI24                                                            
         CLI   PMDEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    RF,PMDLN                                                         
         BXH   R3,RF,INI22                                                      
INI24    MVC   BMEDNM,PMDDESC      SET MEDIA NAME FOR BILLING SOURCE            
         MVC   BMEDIN,PMDCOM1      SET MEDIA RECORD INCOME ACCOUNT              
         DROP  R3,R2                                                            
*                                                                               
INIX     B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* BRANCH TO ROUTINE                                                   *         
***********************************************************************         
         SPACE 1                                                                
ROUT     DS    0H                                                               
         NTR1  ,                                                                
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         L     R8,AROUT(RF)                                                     
         A     R8,BORELO                                                        
         BR    R8                                                               
         SPACE 1                                                                
AROUT    DS    0F                  * ROUTINES *                                 
         DC    A(PRCTRNS)                                                       
         DC    A(TRNGET)                                                        
         DC    A(TRNCST)                                                        
         DC    A(TRNVAT)                                                        
         DC    A(TRNWUP)                                                        
         DC    A(TRNPST)                                                        
         DC    A(TRNIST)                                                        
         DC    A(TRNREP)                                                        
         DC    A(TRNUPD)                                                        
         DC    A(UPDWCT)                                                        
         DC    A(BLDPAS)                                                        
         DC    A(SETVAT)                                                        
         DC    A(INIPST)                                                        
         DC    A(PSTIST)                                                        
         DC    A(PSTBIL)                                                        
         DC    A(PSTINC)                                                        
         DC    A(PSTDEB)                                                        
         DC    A(PSTDSC)                                                        
         DC    A(PSTVAT)                                                        
         DC    A(PSTPST)                                                        
         DC    A(PSTCST)                                                        
         DC    A(PSTWUP)                                                        
         DC    A(PSTREP)                                                        
         DC    A(UPDPRB)                                                        
         DC    A(HDRMNF)                                                        
         DC    A(HDRPCB)                                                        
         DC    A(PSTACCR)                                                       
         DC    A(PSTTRNA)                                                       
         DC    A(PSTDIFF)                                                       
         DC    A(ADDOCA)                                                        
         DC    A(BVATOCA)                                                       
         DC    A(GSTPST)                                                        
         DC    A(GETSCI)                                                        
AROUTN   EQU   (*-AROUT)/L'AROUT                                                
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC=HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC=LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC=EQUAL                                 
*                                                                               
EXIT     XIT1  ,                                                                
         SPACE 1                                                                
***********************************************************************         
* SPACE FOR LITERAL POOL                                              *         
***********************************************************************         
         SPACE 1                                                                
LTORG    DS    0H                                                               
         ORG   CLB1F+X'350'                                                     
LTORGX   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* READ THROUGH AND PROCESS TRANSACTIONS                               *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
PRCTRNS  DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         ZAP   PBCBAPN,BCPZERO     CLEAR NET TOTAL                              
         ZAP   PBCBAPC,BCPZERO     CLEAR COMMISSION TOTAL                       
         ZAP   PBCBALN,BCPZERO     CLEAR NET BILLCURR TOTAL                     
         ZAP   PBCBALC,BCPZERO     CLEAR COM BILLCURR TOTAL                     
*                                                                               
         L     R4,PBADATAB         START OF D/A LIST                            
PTRNS02  ST    R4,PBLSTDA          SAVE FOR NEXT                                
         NI    LSINDS2,FF-LSTOBACC                                              
         CLI   0(R4),FF                                                         
         BE    PTRNS10                                                          
         TM    L'TRNKDA(R4),TRNSBILP                                            
         BZ    PTRNS08             NO BILL ALLOCATION PENDING                   
*                                                                               
         MVC   IODAOVER,0(R4)                                                   
         L     R2,AIO3                                                          
         USING TRNRECD,R2                                                       
*                                                                               
         LA    R1,IOGET+IOACCMST+IO3                                            
         CLI   PBMODE,PBLIVEQ                                                   
         BNE   PTRNS03                                                          
         TM    L'TRNKDA(R4),TYPSADV                                             
         BO    PTRNS03                                                          
         LA    R1,IOLOCK(R1)                                                    
PTRNS03  GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    L'TRNKDA(R4),TYPSADV TEST PC BILLING ADVANCE                     
         BZ    PTRNS04                                                          
         OI    BINDS,BIADV         YES - CONVERT TO TRANSACTION                 
         GOTO1 AADVANCE,BOPARM,=C'CNVTRANS',AIO3                                
*                                                                               
PTRNS04  DS    0H                                                               
         GOTO1 ATRNGET             GET TRANSACTION AND GENERAL DETAILS          
         BNE   EXIT                                                             
         GOTO1 ATRNCST             GET COSTING DETAILS                          
         BNE   EXIT                                                             
         GOTO1 ATRNVAT             VAT DETAILS                                  
         BNE   EXIT                                                             
         GOTO1 ATRNWUP             WRITE-UPS                                    
         BNE   EXIT                                                             
         GOTO1 ATRNPST             PST                                          
         BNE   EXIT                                                             
         GOTO1 ATRNIST             INCOME SUSPENSE TRANSERS                     
         BNE   EXIT                                                             
         GOTO1 ATRNREP             REVERSE ESTIMATED PRODUCTION                 
         BNE   EXIT                                                             
*                                                                               
         CLI   PBMODE,PBLIVEQ      TEST LIVE                                    
         BNE   PTRNS08                                                          
         TM    L'TRNKDA(R4),TYPSADV  AND NOT ADVANCE                            
         BO    PTRNS08                                                          
         GOTO1 ATRNUPD             YES UPDATE TRANSACTION                       
*                                                                               
PTRNS06  DS    0H                  WRITE BACK TRANSACTION RECORD                
K        USING TRNRECD,IOKEY                                                    
         MVC   K.TRNKEY,TRNKEY                                                  
         GOTO1 AIO,IORDUPD+IOACCDIR+IO3                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    K.TRNKSTA2,FF-TRNSBILP                                           
         GOTO1 AIO,IOWRITE+IOACCDIR+IO3                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOPUT+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ABLDPAS,K.TRNKDA                                                 
         GOTO1 AUPDWCT                                                          
         DROP  K                                                                
*                                                                               
PTRNS08  L     R4,PBLSTDA                                                       
         LA    R4,L'TRNKDA+L'TRNKSTA2(R4)                                       
         B     PTRNS02                                                          
*                                                                               
PTRNS10  DS    0H                                                               
         CLI   PBMODE,PBLIVEQ      TEST LIVE                                    
         BNE   PRCTRNSX                                                         
         TM    BINDS,BIADV         TEST PC BILLING ADVANCES                     
         BZ    PRCTRNSX                                                         
*                                                                               
         L     R6,AADTBLK                                                       
         USING TRNBLK,R6                                                        
         GOTO1 AINIADT             INITAILZE ADDTRN BLOCK                       
*                                                                               
         XC    BADVCNT,BADVCNT     CLEAR ADVANCE COUNT                          
         L     R4,PBADATAB                                                      
PTRNS12  CLI   0(R4),FF                                                         
         BE    PTRNS20                                                          
         TM    L'TRNKDA(R4),TYPSADV                                             
         BZ    PTRNS18                                                          
         LH    RF,BADVCNT          INCREMENT ADVANCE COUNT                      
         LA    RF,1(RF)                                                         
         STH   RF,BADVCNT                                                       
         MVC   IODAOVER,0(R4)                                                   
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ATRNUPD                                                          
         GOTO1 AADVANCE,BOPARM,=C'CNVTRANS',AIO3                                
         L     R2,AIO3                                                          
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
         CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TRNMOS,PBBILMC      SET BATCH MOA/REF                            
         MVC   TRNBREF,PBBILREF                                                 
*                                                                               
         MVC   TRNREC,AIO3         SET A(TRANSACTION RECORD)                    
         MVI   TRNINDS,TRNIDRFT+TRNICONV                                        
         XC    TRNCACNM,TRNCACNM                                                
         MVC   TRNBMOS,PBBILMP                                                  
         GOTO1 VADDTRN,ADDTRND     ADD NEW POSTING                              
         BE    *+6                                                              
         DC    H'0'                                                             
K        USING TRNRECD,IOKEY       READ ACCDIR RECORD TO GET NEW D/A            
         L     RF,AIO3                                                          
         MVC   K.TRNKEY,0(RF)                                                   
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ABLDPAS,K.TRNKDA                                                 
         DROP  K                                                                
*                                                                               
PTRNS18  DS    0H                                                               
         LA    R4,L'TRNKDA+L'TRNKSTA2(R4)                                       
         B     PTRNS12                                                          
*                                                                               
PTRNS20  DS    0H                  ADDTRN FINISHED                              
         MVI   TRNINDS,TRNILAST                                                 
         GOTO1 VADDTRN,ADDTRND                                                  
*                                                                               
         MVC   IODAOVER,BCJOBDA    UPDATE JOB'S ADVANCE COUNT                   
         GOTO1 AIO,IOGETRUP+IOACCMST+IO3                                        
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('JCBELQ',AIO3),0                    
         L     R3,12(R1)                                                        
         USING JCBELD,R3                                                        
         CLI   12(R1),0                                                         
         BE    PTRNS22                                                          
         LA    R3,BOELEM                                                        
         XC    JCBELD(JCBLNQ),JCBELD                                            
         MVI   JCBEL,JCBELQ                                                     
         MVI   JCBLN,JCBLNQ                                                     
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),AIO3,JCBELD,0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,16(R1)                                                        
*                                                                               
PTRNS22  ICM   RE,3,JCBADV                                                      
         AH    RE,BADVCNT                                                       
         STCM  RE,3,JCBADV                                                      
         DROP  R3                                                               
         GOTO1 AIO,IOPUT+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PRCTRNSX DS    0H                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* GET TRANSACTION DETAILS                                             *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
TRNGET   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         L     R2,AIO3                                                          
         USING TRNRECD,R2                                                       
*                                                                               
         LA    RE,TRNKWORK                                                      
         CLC   TRNKWORK,=C'**'                                                  
         BNE   TGET06                                                           
         SR    R0,R0                                                            
         LA    R1,TRNRFST-TRNRECD(R2)                                           
TGET04   IC    R0,OAMLN-OAMELD(R1)                                              
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BE    TGET06                                                           
         CLI   0(R1),OAMELQ                                                     
         BNE   TGET04                                                           
         LA    RE,(OAMWORK-OAMELD)(R1)                                          
*                                                                               
TGET06   CLC   BLSTWC,0(RE)        TEST HAD THIS WORKCODE BEFORE                
         MVC   BLSTWC,0(RE)                                                     
         BE    TGET12                                                           
         GOTO1 AGETOPT,BODMCB,AIO3                                              
*                                                                               
         MVI   BWCSRG,C'Y'         SET THIS WORKCODE SURCHARGABLE               
         MVI   BWCTIM,C'C'         SET THIS WORKCODE NOT TIME                   
*&&UK                                                                           
         LA    RE,IOKEY                                                         
         USING WCORECD,RE          READ WORKCODE FOR SURCHARGE STATUS           
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   WCOKWRK,BLSTWC                                                   
         DROP  RE                                                               
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    TGET08                                                           
         MVC   FVMSGNO,=AL2(AE$MISWC)                                           
         MVC   FVXTRA(L'BLSTWC),BLSTWC                                          
         B     EXITN                                                            
         DC    H'0'                                                             
TGET08   GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING WCOELD,RF                                                        
         SR    R0,R0                                                            
         L     RF,AIO1                                                          
         LA    RF,ACTRFST-ACTRECD(RF)                                           
         B     *+10                                                             
TGET10   IC    R0,WCOLN                                                         
         AR    RF,R0                                                            
         CLI   WCOEL,0             TEST E-O-R                                   
         BE    TGET12                                                           
         CLI   WCOEL,WCOELQ                                                     
         BNE   TGET10                                                           
         TM    WCOSTAT2,WCOSNSCH   TEST WORKCODE NON-SURCHARGABLE               
         BNO   *+8                                                              
         MVI   BWCSRG,C'N'                                                      
         TM    WCOSTAT,WCOSHCOE                                                 
         BNO   *+8                                                              
         MVI   BWCTIM,C'T'         TIME WORKCODE                                
         DROP  RF                                                               
*&&                                                                             
*                                                                               
TGET12   SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    TGET14                                                           
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    TGET14                                                           
         LA    R0,CSEXCVAL                                                      
TGET14   GOTO1 APRORATA,BODMCB,AIO3,AGOPBLK,ACOM,(R0),LSPRATA,0                 
*                                                                               
         CLC   CSBILCUR,BCCPYSEC                                                
         BNE   TRNGETX                                                          
*                                                                               
         XC    PP$AALLO,PP$FALLO   SWAP THE ALLOCATION AMOUNTS                  
         XC    PP$FALLO,PP$AALLO   (SO PP&AALLO/PP$ACOMM = PRIMARY CUR          
         XC    PP$AALLO,PP$FALLO     & PP$FALLO/PP#$FCOMM = SECONDARY)          
*                                                                               
         XC    PP$ACOMM,PP$FCOMM                                                
         XC    PP$FCOMM,PP$ACOMM                                                
         XC    PP$ACOMM,PP$FCOMM                                                
*                                                                               
TRNGETX  DS    0H                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* GET TRANSACTION COSTING DETAILS                                     *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
TRNCST   DS    0H                                                               
         USING *,R8                                                             
         L     R2,AIO3                                                          
         USING TRNRECD,R2                                                       
*                                                                               
         XC    TLKEY,TLKEY         GET WORKCODE TSAR RECORD                     
         MVI   TLKSES,TLKUWCQ                                                   
         L     RF,AGOPBLK                                                       
*&&UK                                                                           
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         MVC   TLKUACT,GOINCAC+1-GOBBLOCK(RF)                                   
*&&                                                                             
*&&US                                                                           
         L     RF,AGOPBLK                                                       
         MVC   TLKUACT,GOTBC+1-GOBLOCKD(RF)                                     
         CLC   TLKUACT,BCSPACES                                                 
         BH    *+10                                                             
         MVC   TLKUACT,BMEDIN+1                                                 
*&&                                                                             
         MVC   TLKUWC,TRNKWORK                                                  
         GOTO1 ATSARIO,TSARDH                                                   
         BE    TCST04              TEST ON FILE                                 
         LA    R3,IOKEY                                                         
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,BCSPACES     READ INCOME ACCOUNT FOR COST BYTE            
         L     RF,AGOPBLK                                                       
*&&UK                                                                           
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         MVC   ACTKCULA,GOINCAC-GOBBLOCK(RF)                                    
*&&                                                                             
*&&US                                                                           
         L     RF,AGOPBLK                                                       
         MVC   ACTKCULA,GOTBC-GOBLOCKD(RF)                                      
         CLC   ACTKCULA,BCSPACES                                                
         BH    *+10                                                             
         MVC   ACTKCULA,BMEDIN                                                  
*&&                                                                             
         GOTO1 AGETACT,0                                                        
         BE    TCST02                                                           
         CLI   PBMODE,PBLIVEQ                                                   
         BNE   EXITN                                                            
         DC    H'0'                TOO LATE FOR AN ERROR MESSAGE                
TCST02   XC    TLKEY,TLKEY         BUILD NEW RECORD                             
         XC    TLDU,TLDU           CLEAR RECORD                                 
         MVC   TLRLEN,=Y(TLUPDLNQ)                                              
         MVI   TLKSES,TLKUWCQ                                                   
         MVC   TLKUWC,TRNKWORK                                                  
         MVC   TLKUACT,IOKEY+1                                                  
         L     RF,AGOPBLK                                                       
         MVC   TLDUVT1,GOTAXCOD-GOBLOCK(RF)                                     
         ZAP   BODUB3,PP$AALLO     SET GROSS AS VATABLE                         
         AP    BODUB3,PP$ACOMM                                                  
         GOTO1 ASETVAT             SET VATABLE AMOUNT IN BODUB3                 
         ZAP   TLDUVAM1,BODUB3                                                  
         MVC   TLDUANAL,BCSPACES   CLEAR COSTING ANALYSIS                       
*&&UK*&& MVC   TLDUANAL(L'ACCOST),ACCOST                                        
*&&US                                                                           
         ICM   RF,15,ACASPA                                                     
         BZ    *+10                                                             
         MVC   TLDUANAL,SPAAULA-SPAELD(RF)                                      
*&&                                                                             
         MVC   TLDUACOM,PP$ACOMM                                                
         MVC   TLDUFCOM,PP$FCOMM                                                
         MVC   TLDUANET,PP$AALLO                                                
         MVC   TLDUFNET,PP$FALLO                                                
         AP    PBTOTFN,PP$FALLO    THIS TOTAL NEEDED FOR 99 POSTING             
         LA    R1,TSAADD                                                        
         B     TCST12                                                           
TCST04   AP    TLDUACOM,PP$ACOMM   UPDATE EXISTING RECORD                       
         AP    TLDUFCOM,PP$FCOMM                                                
         AP    TLDUANET,PP$AALLO                                                
         AP    TLDUFNET,PP$FALLO                                                
         AP    PBTOTFN,PP$FALLO    THIS TOTAL NEEDED FOR 99 POSTING             
         ZAP   BODUB3,PP$AALLO     SET GROSS AS VATABLE                         
         AP    BODUB3,PP$ACOMM                                                  
         GOTO1 ASETVAT             GET VATABLE AMOUNT IN BODUB3                 
         L     RF,AGOPBLK                                                       
         LA    RE,TLDUVT1                                                       
         LA    R0,TLDUVTMX                                                      
TCST06   CLI   0(RE),0                                                          
         BE    TCST08                                                           
         CLC   0(L'TLDUVT1,RE),GOTAXCOD-GOBLOCK(RF)                             
         BE    TCST10                                                           
         LA    RE,(TLDUVT2-TLDUVT1)(RE)                                         
         BCT   R0,TCST06                                                        
         DC    H'0'                                                             
TCST08   MVC   0(L'TLDUVT1,RE),GOTAXCOD-GOBLOCK(RF)                             
         ZAP   L'TLDUVT1(L'TLDUVAM1,RE),BCPZERO                                 
TCST10   AP    L'TLDUVT1(L'TLDUVAM1,RE),BODUB3                                  
         LA    R1,TSAWRT                                                        
TCST12   GOTO1 ATSARIO,(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    PBCBAPN,PP$AALLO    ACCUMULATE AGYCURR NET TOTAL                 
         AP    PBCBAPC,PP$ACOMM    ACCUMULATE AGYCURR COMM TOTAL                
         LA    RF,PP$AALLO         AGENCY NET ALLOCATION                        
         CLC   CSBILCUR,CSCPYCUR                                                
         BE    TCST14                                                           
*        CLC   BCCPYSEC,CSBILCUR                                                
*        BE    TCST14                                                           
         LA    RF,PP$FALLO         BILLING NET ALLOCATION                       
TCST14   AP    PBCBALN,0(L'PP$AALLO,RF)                                         
         AP    PBCBALC,PP$ACOMM-PP$AALLO(L'PP$AALLO,RF)                         
         AP    PBCBAPD,PP$ADSCB    ACCUMULATE AGYCURR DISC TOTAL                
         MVC   BSANAL,TLDUANAL     SAVE COSTING CODE                            
*                                                                               
         CLC   BSANAL,BCSPACES     TEST ANY COSTING CODE                        
         BNH   TRNCSTX                                                          
         XC    TLKEY,TLKEY         GET COSTING TSAR RECORD                      
         MVI   TLKSES,TLKUCSTQ                                                  
         MVC   TLKUCOST,BSANAL                                                  
         GOTO1 ATSARIO,TSARDH                                                   
         BE    TCST16                                                           
         XC    TLKEY,TLKEY         BUILD NEW RECORD                             
         XC    TLDU,TLDU           CLEAR RECORD                                 
         MVC   TLRLEN,=Y(TLUPDLNQ)                                              
         MVI   TLKSES,TLKUCSTQ                                                  
         MVC   TLKUCOST,BSANAL                                                  
         MVC   TLDUCNET,PP$AALLO                                                
         MVC   TLDUCCOM,PP$ACOMM                                                
         MVC   TLDUCNEF,PP$FALLO                                                
         MVC   TLDUCCOF,PP$FCOMM                                                
         LA    R1,TSAADD           ADD NEW TSAR RECORD                          
         B     TCST18                                                           
TCST16   AP    TLDUCNET,PP$AALLO                                                
         AP    TLDUCCOM,PP$ACOMM                                                
         AP    TLDUCNEF,PP$FALLO                                                
         AP    TLDUCCOF,PP$FCOMM                                                
         LA    R1,TSAWRT           WRITE BACK UPDATED RECORD                    
TCST18   GOTO1 ATSARIO,(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TRNCSTX  B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ADD TRANSACTION VAT DETAILS                                         *         
***********************************************************************         
         SPACE 1                                                                
TRNVAT   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         TM    BCGLOB1,BCGLVAT                                                  
         BZ    TRNVATX                                                          
         XC    TLKEY,TLKEY         GET VAT TYPE TSAR RECORD                     
         MVI   TLKSES,TLKUVATQ                                                  
         L     RF,AGOPBLK                                                       
         MVC   TLKUVATT,GOTAXCOD-GOBLOCK(RF)                                    
         GOTO1 ATSARIO,TSARDH                                                   
         BE    TVAT02                                                           
         XC    TLKEY,TLKEY         BUILD NEW RECORD                             
         MVC   TLRLEN,=Y(TLUPDLNQ)                                              
         MVI   TLKSES,TLKUVATQ                                                  
         L     RF,AGOPBLK                                                       
         MVC   TLKUVATT,GOTAXCOD-GOBLOCK(RF)                                    
         ZAP   TLDUVATA,PP$AALLO   VATABLE IS NET PLUS COMM                     
         AP    TLDUVATA,PP$ACOMM                                                
         ZAP   TLDUVTFA,PP$FALLO   VATABLE IN FOREIGN CURR                      
         AP    TLDUVTFA,PP$FCOMM                                                
         ZAP   TLDUVCOM,PP$ACOMM                                                
         ZAP   TLDUVAT,BCPZERO     INITIALISE VAT AMOUNT FIELD                  
         ZAP   TLDUVTF,BCPZERO     INITIALISE VAT FOREIGN                       
         ZAP   TLDUVVSG,BCPZERO                                                 
         ZAP   TLDUVFSG,BCPZERO                                                 
         CLI   BWCSRG,C'Y'                                                      
         BNE   *+16                                                             
         MVC   TLDUVVSG,TLDUVATA   SURCHARGABLE AMOUNT                          
         MVC   TLDUVFSG,TLDUVTFA   SURCHARGABLE AMOUNT IN CURRENCY              
         LA    R1,TSAADD                                                        
         B     TVAT06                                                           
*                                                                               
TVAT02   AP    TLDUVATA,PP$AALLO   UPDATE EXISTING RECORD                       
         AP    TLDUVATA,PP$ACOMM                                                
         AP    TLDUVTFA,PP$FALLO   VATABLE IN FOREIGN CURR                      
         AP    TLDUVTFA,PP$FCOMM                                                
         AP    TLDUVCOM,PP$ACOMM                                                
         CLI   BWCSRG,C'Y'                                                      
         BNE   TVAT04                                                           
         AP    TLDUVVSG,PP$AALLO                                                
         AP    TLDUVVSG,PP$ACOMM                                                
         AP    TLDUVFSG,PP$FALLO                                                
         AP    TLDUVFSG,PP$FCOMM                                                
TVAT04   LA    R1,TSAWRT                                                        
TVAT06   GOTO1 ATSARIO,(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TRNVATX  B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ADD TRANSACTION WRITE-UP DETAILS                                    *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
TRNWUP   DS    0H                                                               
         USING *,R8                                                             
         L     R2,AIO3                                                          
         USING TRNRECD,R2                                                       
*                                                                               
         XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,TLKUWUPQ                                                  
         ZAP   BODUB1,PP$WUAMT                                                  
         ZAP   BODUB2,PP$WUHRS                                                  
         L     RE,AIO3                                                          
         CLI   TRNRSTYP-TRNRECD(RE),8                                           
         BE    TWUP02                                                           
         CP    PP$WUAMT,BCPZERO    TEST ANY WRITE-UP AMOUNT                     
         BE    TRNWUPX                                                          
         CP    PP$WUHRS,BCPZERO    TEST IF WRITE-UP HAD HOURS                   
         BE    TWUP08                                                           
*                                                                               
         MVC   TLKUPERS,TRNKULC                                                 
         LA    R3,TRNKULC                                                       
         CLC   UL1R,TLKUPERS                                                    
         BE    TWUP04                                                           
         CLC   UL1P,TLKUPERS                                                    
         BE    TWUP04                                                           
*                                                                               
TWUP02   GOTO1 VHELLO,BODMCB,(C'G',ACCMST),('SPAELQ',TRNRECD),         X        
               (L'SPATYPE,=AL1(SPATPERS))                                       
         CLI   BODMCB+12,0                                                      
         BNE   TWUP08                                                           
         AP    BODUB1,PP$AALLO                                                  
         AP    BODUB2,PP$HRSB                                                   
         L     R3,BODMCB+12                                                     
         LA    R3,SPAAULA-SPAELD(R3)                                            
         MVC   TLKUPERS,0(R3)                                                   
TWUP04   GOTO1 ATSARIO,TSARDH                                                   
         BE    TWUP06                                                           
         XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,TLKUWUPQ                                                  
         MVC   TLKUPERS,0(R3)                                                   
         ZAP   TLDUWAMT,BCPZERO                                                 
         ZAP   TLDUWHRS,BCPZERO                                                 
         LA    R1,TSAADD                                                        
         B     *+8                                                              
TWUP06   LA    R1,TSAWRT                                                        
         AP    TLDUWAMT,BODUB1     AMOUNT                                       
         AP    TLDUWHRS,BODUB2     HOURS                                        
         GOTO1 ATSARIO,(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TWUP08   CP    PP$WUAMT,BCPZERO    TEST ANY WRITE-UP AMOUNT                     
         BE    TRNWUPX                                                          
         XC    TLKEY,TLKEY         READ FOR THIS WC/ACCOUNT RECORD              
         MVI   TLKSES,TLKUWCQ                                                   
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         MVC   TLKUACT,GOWUPTAC+1-GOBBLOCK(RF)                                  
         CLI   BWCTIM,C'T'                                                      
         BE    *+10                                                             
         MVC   TLKUACT,GOWUPCAC+1-GOBBLOCK(RF)                                  
         MVC   TLKUWC,TRNKWORK                                                  
         GOTO1 ATSARIO,TSARDH                                                   
         BE    TWUP12                                                           
         LA    R3,IOKEY                                                         
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,BCSPACES     READ INCOME ACCOUNT FOR COST BYTE            
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         MVC   ACTKCULA,GOWUPTAC-GOBBLOCK(RF)                                   
         CLI   BWCTIM,C'T'                                                      
         BE    *+10                                                             
         MVC   ACTKCULA,GOWUPCAC-GOBBLOCK(RF)                                   
         GOTO1 AGETACT,0                                                        
         BE    TWUP10                                                           
         CLI   PBMODE,PBLIVEQ                                                   
         BNE   EXITN                                                            
         DC    H'0'                TOO LATE FOR AN ERROR MESSAGE                
TWUP10   L     R1,ATSABLK                                                       
         XC    TLKEY,TLKEY         BUILD NEW RECORD                             
         XC    TLDU,TLDU           CLEAR RECORD                                 
         MVI   TLKSES,TLKUWCQ                                                   
         MVC   TLKUWC,TRNKWORK                                                  
         MVC   TLKUACT,IOKEY+1                                                  
         L     RF,AGOPBLK                                                       
         MVC   TLDUVT1,GOTAXCOD-GOBLOCK(RF)                                     
         ZAP   BODUB3,PP$WUAMT                                                  
         GOTO1 ASETVAT             SET VATABLE AMOUNT IN BODUB3                 
         ZAP   TLDUVAM1,BODUB3                                                  
         MVC   TLDUANAL,BCSPACES   CLEAR COSTING ANALYSIS                       
         MVC   TLDUANAL(L'ACCOST),ACCOST                                        
         ZAP   TLDUACOM,PP$WUAMT                                                
         ZAP   TLDUFCOM,BCPZERO                                                 
         ZAP   TLDUANET,BCPZERO                                                 
         ZAP   TLDUFNET,BCPZERO                                                 
         LA    R1,TSAADD                                                        
         B     TWUP20                                                           
TWUP12   AP    TLDUACOM,PP$WUAMT   UPDATE EXISTING RECORD                       
         ZAP   BODUB3,PP$WUAMT                                                  
         GOTO1 ASETVAT             GET VATABLE AMOUNT INTO BODUB3               
         L     RF,AGOPBLK                                                       
         LA    RE,TLDUVT1                                                       
         LA    R0,TLDUVTMX                                                      
TWUP14   CLI   0(RE),0                                                          
         BE    TWUP16                                                           
         CLC   0(L'TLDUVT1,RE),GOTAXCOD-GOBLOCK(RF)                             
         BE    TWUP18                                                           
         LA    RE,(TLDUVT2-TLDUVT1)(RE)                                         
         BCT   R0,TWUP14                                                        
         DC    H'0'                                                             
TWUP16   MVC   0(L'TLDUVT1,RE),GOTAXCOD-GOBLOCK(RF)                             
         ZAP   L'TLDUVT1(L'TLDUVAM1,RE),BCPZERO                                 
TWUP18   AP    L'TLDUVT1(L'TLDUVAM1,RE),BODUB3                                  
         LA    R1,TSAWRT                                                        
TWUP20   GOTO1 ATSARIO,(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    PBCBAPC,PP$WUAMT    ADD WRITE-UP INTO BILLED COMMISSION          
         AP    PBCBALC,PP$WUAMT    ADD INTO ALLOCATED COMMISSION                
         MVC   BSANAL,TLDUANAL     SAVE COSTING CODE                            
*                                                                               
         CLC   BSANAL,BCSPACES     TEST ANY COSTING CODE                        
         BNH   TWUP26                                                           
         XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,TLKUCSTQ                                                  
         MVC   TLKUCOST,BSANAL                                                  
         GOTO1 ATSARIO,TSARDH                                                   
         BE    TWUP22                                                           
         XC    TLKEY,TLKEY         BUILD NEW RECORD                             
         XC    TLDU,TLDU           CLEAR RECORD                                 
         MVI   TLKSES,TLKUCSTQ                                                  
         MVC   TLKUCOST,BSANAL                                                  
         ZAP   TLDUCNET,BCPZERO                                                 
         ZAP   TLDUCCOM,PP$WUAMT                                                
         ZAP   TLDUCNEF,BCPZERO                                                 
         ZAP   TLDUCCOF,BCPZERO                                                 
         LA    R1,TSAADD           ADD NEW TSAR RECORD                          
         B     TWUP24                                                           
TWUP22   AP    TLDUCCOM,PP$WUAMT                                                
         LA    R1,TSAWRT           WRITE BACK UPDATED RECORD                    
TWUP24   GOTO1 ATSARIO,(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TWUP26   TM    BCGLOB1,BCGLVAT                                                  
         BZ    TRNWUPX                                                          
         XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,TLKUVATQ                                                  
         L     RF,AGOPBLK                                                       
         MVC   TLKUVATT,GOTAXCOD-GOBLOCK(RF)                                    
         GOTO1 ATSARIO,TSARDH                                                   
         BE    TWUP28                                                           
         XC    TLKEY,TLKEY         BUILD NEW RECORD                             
         MVI   TLKSES,TLKUVATQ                                                  
         MVC   TLRLEN,=Y(TLUPDLNQ)                                              
         L     RF,AGOPBLK                                                       
         MVC   TLKUVATT,GOTAXCOD-GOBLOCK(RF)                                    
         MVC   TLDUVATA,PP$WUAMT                                                
         ZAP   TLDUVAT,BCPZERO     INITIALISE VAT AMOUNT FIELD                  
         ZAP   TLDUVVSG,BCPZERO                                                 
         ZAP   TLDUVFSG,BCPZERO                                                 
         CLI   BWCSRG,C'Y'                                                      
         BNE   *+16                                                             
         MVC   TLDUVVSG,TLDUVATA   SURCHARGABLE AMOUNT                          
         MVC   TLDUVFSG,TLDUVTFA   SURCHARGABLE AMOUNT IN CURRENCY              
         LA    R1,TSAADD                                                        
         B     TWUP30                                                           
*                                                                               
TWUP28   AP    TLDUVATA,PP$WUAMT   UPDATE EXISTING RECORD                       
         CLI   BWCSRG,C'Y'                                                      
         BNE   *+10                                                             
         AP    TLDUVVSG,PP$WUAMT                                                
         LA    R1,TSAWRT                                                        
TWUP30   GOTO1 ATSARIO,(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TRNWUPX  DS    0H                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ADD TRANSACTION PST DETAILS                                         *         
***********************************************************************         
         SPACE 1                                                                
TRNPST   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         TM    BCGLOB1,BCGLPST                                                  
         BZ    TRNPSTX                                                          
*&&US                                                                           
         XC    TLKEY,TLKEY         GET PST TYPE TSAR RECORD                     
         MVI   TLKSES,TLKUPSTQ                                                  
         L     RF,AGOPBLK                                                       
         CLC   GOPSTOPT-GOBLOCK(L'GOPSTOPT,RF),BCSPACES                         
         BNH   TPST02                                                           
         MVC   TLKUPSTP,GOPSTPRV-GOBLOCK(RF)                                    
         MVC   TLKUPSTT,GOPSTCOD-GOBLOCK(RF)                                    
         GOTO1 ATSARIO,TSARDH                                                   
         BE    TPST02                                                           
         XC    TLKEY,TLKEY         BUILD NEW RECORD                             
         MVC   TLRLEN,=Y(TLUPDLNQ)                                              
         MVI   TLKSES,TLKUPSTQ                                                  
         L     RF,AGOPBLK                                                       
         MVC   TLKUPSTP,GOPSTPRV-GOBLOCK(RF)                                    
         MVC   TLKUPSTT,GOPSTCOD-GOBLOCK(RF)                                    
         MVC   TLDUPSTA,PP$AALLO   PSTABLE IS NET PLUS COMM                     
         AP    TLDUPSTA,PP$ACOMM                                                
         ZAP   TLDUPCOM,PP$ACOMM                                                
         ZAP   TLDUPST,BCPZERO     INITIALISE PST AMOUNT FIELD                  
         LA    R1,TSAADD                                                        
         B     TPST04                                                           
*                                                                               
TPST02   AP    TLDUPSTA,PP$AALLO   UPDATE EXISTING RECORD                       
         AP    TLDUPSTA,PP$ACOMM                                                
         AP    TLDUPCOM,PP$ACOMM                                                
         L     RF,AGOPBLK                                                       
         LA    R1,TSAWRT                                                        
*                                                                               
TPST04   LA    RE,TLDUGT1                                                       
         LA    R0,TLDUGTMX                                                      
TPST06   CLI   0(RE),0                                                          
         BE    TPST08                                                           
         CLC   0(L'TLDUGT1,RE),GOTAXCOD-GOBLOCK(RF)                             
         BE    TPST10                                                           
         LA    RE,(TLDUGT2-TLDUGT1)(RE)                                         
         BCT   R0,TPST06                                                        
         DC    H'0'                                                             
TPST08   MVC   0(L'TLDUGT1,RE),GOTAXCOD-GOBLOCK(RF)                             
         ZAP   L'TLDUGT1(L'TLDUGAM1,RE),BCPZERO                                 
TPST10   AP    L'TLDUGT1(L'TLDUGAM1,RE),PP$AALLO                                
         AP    L'TLDUGT1(L'TLDUGAM1,RE),PP$ACOMM                                
         GOTO1 ATSARIO,(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
TRNPSTX  B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ADD TRANSACTION INCOME SUSPENSE TRANSFER DETAILS                    *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
TRNIST   DS    0H                                                               
         USING *,R8                                                             
         L     R2,AIO3                                                          
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
         CLI   TRNEL,TRNELQ        FIND ANY INCOME SUSPENSE ACCOUNT             
         BE    *+6                                                              
         DC    H'0'                                                             
*&&US                                                                           
         CLI   TRNTYPE,8           IF INTERNAL INVOICE USE SK CONTRA            
         BE    TIST02                                                           
         CLI   TRNTYPE,34          ALSO TRANSFERS                               
         BE    TIST02                                                           
         CLI   TRNTYPE,62          TYPE 62 AS WELL                              
         BNE   TIST04                                                           
*&&                                                                             
TIST02   CLC   =C'SK',TRNKULC                                                   
         BNE   TIST04              OR TRY SK ATTRIBUTE                          
         LA    R4,TRNKULC                                                       
         B     TIST12                                                           
*                                                                               
         USING APEELD,RF           FIND SK ATTRIBUTE                            
TIST04   LA    RF,TRNEL                                                         
         SR    R0,R0                                                            
TIST06   IC    R0,APELN                                                         
         AR    RF,R0                                                            
         CLI   APEEL,0             NO SK ATTRIBUTE                              
         BE    TRNISTX                                                          
         CLI   APEEL,APEELQ                                                     
         BE    TIST08                                                           
         CLI   APEEL,SPDELQ                                                     
         BE    TIST10                                                           
         B     TIST06                                                           
TIST08   CLI   APELN,APENACT-APEELD                                             
         BNH   TRNISTX                                                          
         CLC   =C'SK',APENACT                                                   
         BNE   TIST06                                                           
         LA    R4,BOWORK1                                                       
         MVC   0(L'APENACT,R4),BCSPACES                                         
         IC    RE,APELN                                                         
         SH    RE,=Y(APENACT+1-APEELD)                                          
         EX    RE,*+8                                                           
         B     TIST12                                                           
         MVC   0(0,R4),APENACT                                                  
         DROP  RF                                                               
         USING SPDELD,RF                                                        
TIST10   CLC   =C'SK',SPDACCS                                                   
         BNE   TIST06                                                           
         LA    R4,BOWORK1                                                       
         MVC   0(L'APENACT,R4),BCSPACES                                         
         IC    RE,SPDLN                                                         
         SH    RE,=Y(SPDACCS+1-SPDELD)                                          
         EX    RE,*+4                                                           
         MVC   0(0,R4),SPDACCS                                                  
         DROP  RF                                                               
*                                                                               
TIST12   XC    TLKEY,TLKEY         GET TRANSFER RECORD                          
         MVI   TLKSES,TLKUTRFQ                                                  
         MVC   TLKUTACT,0(R4)                                                   
         MVC   TLKUTWRK,TRNKWORK                                                
         MVC   TLKUTDAT,TRNKDATE                                                
         MVC   TLKUTREF,TRNKREF                                                 
         GOTO1 ATSARIO,TSARDH                                                   
         BE    TIST20                                                           
         XC    TLKEY,TLKEY         BUILD NEW RECORD                             
         MVC   TLRLEN,=Y(TLUPDLNQ)                                              
         MVI   TLKSES,TLKUTRFQ                                                  
         MVC   TLKUTACT,0(R4)                                                   
         MVC   TLKUTWRK,TRNKWORK                                                
         MVC   TLKUTDAT,TRNKDATE                                                
         MVC   TLKUTREF,TRNKREF                                                 
         ZAP   TLDUTRFA,PP$AALLO   TRANSFER AMOUNT IS NET ALLOCATION            
         ZAP   TLDUTRFF,PP$FALLO   SECOND CURRENCY AMOUNT                       
         MVC   TLDUTPER,BCSPACES   SET PERSON TO SPACES                         
         CLC   =C'1R',TRNKULC                                                   
         BNE   *+10                                                             
         MVC   TLDUTPER,TRNKULC                                                 
*&&US*&& AP    PBTOTAD,PP$ADSCB                                                 
         LA    R1,TSAADD                                                        
         B     TIST22                                                           
*                                                                               
TIST20   AP    TLDUTRFA,PP$AALLO   UPDATE EXISTING RECORD                       
         AP    TLDUTRFF,PP$FALLO   SECOND CURRENCY AMOUNT                       
*&&US*&& AP    PBTOTAD,PP$ADSCB                                                 
         LA    R1,TSAWRT                                                        
TIST22   GOTO1 ATSARIO,(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TRNISTX  B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ADD TRANSACTION REVERSE ESTIMATED PRODUCTION DETAILS                *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
TRNREP   DS    0H                                                               
         USING *,R8                                                             
         L     R2,AIO3                                                          
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
*                                                                               
         CLI   TRNTYPE,47          TEST THIS ITEM IS ESTIMATED PROD             
         BNE   TRNREPX                                                          
         XC    TLKEY,TLKEY         BUILD NEW RECORD (CAN ONLY BE ONE)           
         MVC   TLRLEN,=Y(TLUPDLNQ)                                              
         MVI   TLKSES,TLKUREPQ                                                  
         MVC   TLKURWRK,TRNKWORK                                                
         MVC   TLKURCON,TRNKULC                                                 
         MVC   TLKURDAT,TRNKDATE                                                
         MVC   TLKURREF,TRNKREF                                                 
         MVC   TLDURCRC,BCPROCOD                                                
         ZAP   TLDUREPA,PP$AALLO   ESTIMATED AMOUNT IS NET ALLOCATION           
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('SPDELQ',AIO3),0,0                  
         CLI   12(R1),0                                                         
         BNE   TREP02                                                           
         MVC   TLDURCRC,BCSPACES   USE A/C IN SPDEL FOR CREDIT CONTRA           
         L     RE,12(R1)                                                        
         SR    RF,RF                                                            
         IC    RF,SPDLN-SPDELD(RE)                                              
         SH    RF,=Y(SPDACCS+1-SPDELD)                                          
         EX    RF,*+4                                                           
         MVC   TLDURCRC(0),SPDACCS-SPDELD(RE)                                   
TREP02   LA    R1,TSAADD                                                        
         GOTO1 ATSARIO,(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TRNREPX  B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* UPDATE TRANSACTION                                                  *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
TRNUPD   DS    0H                                                               
         USING *,R8                                                             
         L     R2,AIO3                                                          
         USING TRNRECD,R2                                                       
         LA    R3,TRNRFST                                                       
         USING PTAELD,R3                                                        
         XR    RF,RF                                                            
         XC    BAPTAEL,BAPTAEL                                                  
*                                                                               
TUPD02   CLI   PTAEL,0                                                          
         BE    TUPD10                                                           
         CLI   PTAEL,PTAELQ                                                     
         BNE   TUPD08                                                           
         CLI   PTATYPE,PTATRAL     TEST ALLOCATION ELEMENT                      
         BNE   TUPD08                                                           
         TM    PTASTAT1,PTASPEND   TEST ACTIVITY PENDING                        
         BO    TUPD04                                                           
         TM    PTASTAT1,PTASREVD   TEST THIS PTAEL BEING REVERSED               
         BNO   TUPD08                                                           
         OI    PTASTAT1,PTASREVU   THEN SET AS UPDATED                          
         OI    BINDS,BIREVBL       SET REVERSAL INDICATOR                       
         B     TUPD08                                                           
*                                                                               
TUPD04   DS    0H                                                               
         NI    PTASTAT1,FF-PTASPEND                                             
         MVC   PTARDATE,BCTODAYC   BILLING DATE (TODAY)                         
         MVC   PTARPERS,CUPASS     PERSON CODE                                  
         MVC   PTARBLNO,PBLIVE#    BILL NUMBER (LIVE)                           
         MVC   PTARBLDT,PBDATC     BILL DATE                                    
         MVC   PTAMOA,PBBILMP      MONTH OF BILL                                
         MVC   PTAOFFC,CSOFFICE    SET OFFICE FOR DEBTOR PASSIVE                
         NI    TRNRSTA2,FF-TRNSBILP                                             
         ST    R3,BAPTAEL          SET PENDING ELEMENT FOUND                    
*                                                                               
TUPD08   IC    RF,PTALN                                                         
         BXH   R3,RF,TUPD02                                                     
*                                                                               
TUPD10   DS    0H                                                               
         OC    BAPTAEL,BAPTAEL     TEST PENDING ELEMENT FOUND                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   PBMODE,PBLIVEQ      TEST ACTION IS LIVE                          
         BNE   TRNUPDX                                                          
         GOTO1 VHELLO,BODMCB,(C'G',ACCMST),('TRXELQ',AIO3),0,0                  
         CLI   BODMCB+12,0                                                      
         BNE   *+12                                                             
         L     RE,BODMCB+12                                                     
         NI    TRXSTA2-TRXELD(RE),FF-TRXSBILP                                   
*                                  UPDATE TRANSACTION RECORD                    
         TM    LSINDS2,LSTOBACC    TEST RECORD IS CONVERTED                     
         BNO   TRNUPDX                                                          
         NI    LSINDS2,FF-LSTOBACC                                              
         MVC   BCWORK(L'CSCPYCUR),CSBILCUR                                      
         MVC   BCWORK+L'CSCPYCUR(L'CSBILCUR),CSCPYCUR                           
         GOTO1 VTOBACCO,BODMCB,('TOBAACVB',BCWORK),AIO3,ACOM,0,0                
*                                                                               
TRNUPDX  DS    0H                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
**********************************************************************          
* UPDATE WORK-CODE SUB-TOTALS  (WITHIN MAINFRAM PROG LISTS)          *          
**********************************************************************          
         SPACE 1                                                                
UPDWCT   DS    0H                                                               
         USING *,R8                                                             
         CLI   CSACT,ACTDWN        NO NEED FOR PC BILLING                       
         BE    EXITY                                                            
*                                                                               
         CLC   BCCPYSEC,CSBILCUR   TEST AMOUNTS SWAPPED                         
         BNE   UWCT02                                                           
         XC    PP$AALLO,PP$FALLO   SWAP THEM BACK                               
         XC    PP$FALLO,PP$AALLO                                                
         XC    PP$AALLO,PP$FALLO                                                
*                                                                               
         XC    PP$ACOMM,PP$FCOMM                                                
         XC    PP$FCOMM,PP$ACOMM                                                
         XC    PP$ACOMM,PP$FCOMM                                                
*                                                                               
UWCT02   XR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    UWCT04                                                           
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    UWCT04                                                           
         LA    R0,CSEXCVAL                                                      
UWCT04   GOTO1 APRORATA,BODMCB,AIO3,AGOPBLK,ACOM,(R0),LSPRATAS,0                
         GOTO1 ASETTRN,BODMCB,(C'S',IOKEY),AIO3,LSPRATA                         
         GOTO1 ASUBTOT,BOPARM,(C'U',LSPRATA),LSPRATAS                           
*                                                                               
UPDWCTX  B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD PASSIVE                                                       *         
*                                                                     *         
* NTRY: R1 = A(DISK ADDRESS OF TRANSACTION)                           *         
***********************************************************************         
         SPACE 1                                                                
BLDPAS   DS    0H                                                               
         USING *,R8                                                             
*                                  BUILD PASSIVE                                
*&&UK                                                                           
         ICM   RF,15,0(R1)         RF = DISK ADDRESS                            
         PUSH  USING                                                            
         USING BDPPASD,IOKEY                                                    
         XC    BDPPKEY,BDPPKEY                                                  
         MVI   BDPPTYP,BDPPTYPQ                                                 
         MVC   BDPPCPY,CUABIN                                                   
         MVI   BDPPSYS,BDPPACCQ                                                 
         MVC   BDPPOFF,CSOFFICE                                                 
         MVC   BDPPJOB,BCJOBCOD                                                 
         MVC   BDPPBILM,PBBILMP                                                 
         MVC   BDPPBIL#,PBLIVE#                                                 
         MVC   BDPPBILD,PBDATC                                                  
         MVI   BDPPRTYP,BDPPRWDR                                                
         STCM  RF,15,BDPPDA        PUT D/A IN KEY OF RECORD                     
         MVC   BDPPDA,BDPPKDA                                                   
         XC    BDPPKSTA,BDPPKSTA   CLEAR STATUS AREA                            
         STCM  RF,15,BDPPKDA                                                    
         GOTO1 AIO,IOADD+IOACCDIR                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*                                                                               
BLDPASX  B     EXITY                                                            
                                                                                
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* GET VATABLE AMOUNT FOR THIS ITEM                                    *         
***********************************************************************         
         SPACE 1                                                                
SETVAT   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         CP    PBSRCPC,BCPZERO     TEST ANY SURCHARGE RATE                      
         BE    SETVAT2                                                          
         CLI   BWCSRG,C'N'         TEST WORKCODE NON-SURCHARGABLE               
         BE    SETVAT2                                                          
         CLI   P#VATSCH,C'Y'       TEST SURCHARGE VATABLE                       
         BNE   SETVAT2                                                          
         ZAP   BODUB1(2*L'BODUB1),BODUB3                                        
         MP    BODUB1(2*L'BODUB1),PBSRCPC                                       
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         AP    BODUB3,BODUB2       ADD SURCHARGE TO VATABLE                     
*                                                                               
SETVAT2  CP    PBDSCPC,BCPZERO     TEST ANY DISCOUNT                            
         BE    SETVATX                                                          
         ZAP   BODUB1(2*L'BODUB1),PP$AALLO                                      
         AP    BODUB1(2*L'BODUB1),PP$ACOMM                                      
         MP    BODUB1(2*L'BODUB1),PBDSCPC                                       
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         SP    BODUB3,BODUB2       VATABLE IS ALWAYS DISCOUNTED                 
SETVATX  B     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
* INITIALIZE FOR POSTING ELEMENTS                                    *          
**********************************************************************          
         SPACE 1                                                                
INIPST   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         OC    PBTADVN,PBTADVN                                                  
         BNZ   *+10                                                             
         ZAP   PBTADVN,BCPZERO                                                  
         OC    PBTADVC,PBTADVC                                                  
         BNZ   *+10                                                             
         ZAP   PBTADVC,BCPZERO                                                  
*                                                                               
         CLI   PBMODE,PBTOTQ                                                    
         BE    IPST02                                                           
         CP    PBBHAPN,PBCBALN     TEST NET & COMM ARE STILL THE SAME           
         BE    *+6                                                              
         DC    H'0'                                                             
         CP    PBBHAPC,PBCBALC                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
IPST02   DS    0H                                                               
         TM    BCINDS1,BCPCBILL    TEST PC BILLING                              
         BZ    IPST03                                                           
         ICM   RF,15,ALINK         PASS REPORT ID (IF HAVE IT)                  
         BZ    IPST03                                                           
         MVC   CSREPID,BEWPSTPQ-LINKD(RF)                                       
IPST03   GOTO1 AOPNJRN             OPEN JOURNAL (NOW THE TIA IS FREE)           
*                                                                               
         CLI   PBMODE,PBLIVEQ      TEST LIVE                                    
         BNE   IPST04                                                           
         TM    PBINDS1,PBIAUTO     TEST BILL NUMBER AUTO SET                    
         BZ    IPST04                                                           
         LHI   RE,IOAREA5-WORKD    RESTORE IO5                                  
         LA    RE,WORKD(RE)                                                     
         LHI   RF,IOAREALN                                                      
         LA    R0,BIOAREA5                                                      
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         L     R1,=A(IOPUTREC+IOACCMST+IO5)                                     
         GOTO1 AIO                                                              
         BE   *+6                                                               
         DC    H'0'                                                             
*                                  INITIALISE REPORT AND ADDTRN                 
IPST04   MVI   POSTTYPE,POSTBILL   SET TRNTYPE                                  
         MVC   POSTCAC,BCSPACES                                                 
         MVC   POSTCACN,BCSPACES                                                
         MVC   POSTBTMC,PBBILMC    SET CHARACTER MONTH FOR BATCH REF            
         MVC   POSTBTRF,PBBILREF   SET BATCH REFERENCE                          
         MVC   POSTDUED,PBDUE                                                   
         XC    BVRATT(TLDUVTMX*L'BVRATT),BVRATT                                 
*                                                                               
INIPSTX  B     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
*              BUILD SK/SI INCOME SUSPENSE TRANSFERS                 *          
**********************************************************************          
         SPACE 1                                                                
PSTIST   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         XC    TLKEY,TLKEY         FIRST OF ALL GET A NON-ZERO VAT ACC          
         MVI   TLKSES,TLKUVATQ                                                  
         LA    R1,TSARDH                                                        
         B     *+8                                                              
PIST02   LA    R1,TSANXT                                                        
         GOTO1 ATSARIO,(R1)                                                     
         BL    PIST04                                                           
         CLI   TLKSES,TLKUVATQ                                                  
         BNE   PIST04              END OF VAT RECORDS                           
         PUSH  USING                                                            
         L     R1,AIO1                                                          
         USING VTCD,R1                                                          
         XC    VTCD(VTCLNQ),VTCD                                                
         MVI   VTCACTN,VTCALOOK                                                 
         MVC   VTCCPY,CUABIN                                                    
         TM    BCCPYST1,CPYSOROE   TEST ON OFFICES                              
         BNO   *+10                                                             
         MVC   VTCOFFC,CSOFFICE                                                 
         MVC   VTCTYPE,TLKUVATT                                                 
         MVC   VTCCOMF,ACOM                                                     
         MVC   VTCINVD,PBDATP                                                   
         OC    PBDATVP,PBDATVP                                                  
         BZ    *+10                                                             
         MVC   VTCINVD,PBDATVP                                                  
         GOTO1 VVATICAN                                                         
         OC    VTCRATE,VTCRATE                                                  
         BZ    PIST02                                                           
         MVC   BVATAC,VTCACT+1     SAVE THE POSTING ACCOUNT                     
         DROP  R1                                                               
         POP   USING                                                            
*                                                                               
PIST04   XC    TLKEY,TLKEY         GET SK/SI TRANSFER RECORDS                   
         MVI   TLKSES,TLKUTRFQ                                                  
         LA    R1,TSARDH                                                        
         B     *+8                                                              
PIST06   LA    R1,TSANXT                                                        
         GOTO1 ATSARIO,(R1)                                                     
         BL    PSTISTX                                                          
         CLI   TLKSES,TLKUTRFQ                                                  
         BNE   PSTISTX             NO MORE TRANSFERS                            
         MVC   POSTACT,TLKUTACT                                                 
         MVC   POSTCAC(L'TRNKCPY),CUABIN                                        
         MVC   POSTCAC+L'CUABIN(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)             
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCJOBCOD),BCJOBCOD                  
         MVC   POSTCACN,BCJOBNAM                                                
         MVC   POSTDATE,PBDATP     TRANSACTION DATE IS BILL DATE                
         MVC   POSTREF,PBLIVE#     TRANSACTION REF IS LIVE BILL NUM             
         OC    POSTREF,POSTREF     OR DRAFT BILL NUM                            
         BNZ   *+10                                                             
         MVC   POSTREF,PBDRAFT#                                                 
         MVI   POSTSTAT,TRNSDR     DEBIT SK                                     
         ZAP   POSTAMNT,TLDUTRFA                                                
         AP    PBTOTSK,POSTAMNT    KEEP SK TOTAL FOR USA MDTEL                  
         MVC   POSTOFFC,CSOFFICE                                                
         MVI   PBPTRS,FF                                                        
         GOTO1 AADDOCA,BODMCB,('QTRNAMNT',TLDUTRFF),(X'FF',0)                   
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         MVC   POSTACT(ACTKACT-ACTKUNT),=C'SI'                                  
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCPROCOD),BCPROCOD                  
         MVC   POSTCACN,BCPRONAM                                                
*                                                                               
         LA    R3,IOKEY                                                         
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,BCSPACES     READ INCOME ACCOUNT FOR COST BYTE            
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,POSTACT                                                  
         GOTO1 AGETACT,0                                                        
         BE    *+6                                                              
         DC    H'0'                IT MUST BE THERE                             
         DROP  R3                                                               
         MVC   BSICST,BCSPACES                                                  
*&&UK                                                                           
         CLI   ACCOST,C' '         TEST ANY ANALYSIS                            
         BNH   *+16                                                             
         MVC   BSICST(L'ULREV),ULREV                                            
         MVC   BSICST+L'ULREV(L'ACCOST),ACCOST                                  
*&&                                                                             
*&&US                                                                           
         ICM   RF,15,ACASPA                                                     
         BZ    *+16                                                             
         MVC   BSICST(L'ULREV),ULREV                                            
         MVC   BSICST+L'ULREV(L'ACTKACT),SPAAULA-SPAELD(RF)                     
*&&                                                                             
*&&US                                                                           
         OC    BSANAC,BSANAC                                                    
         BZ    *+16                                                             
         MVC   POSTCAC,BSANAC                                                   
         MVC   POSTCACN,BSANNM                                                  
         GOTO1 AADDRFL,TLKUTWRK    ADD WORKCODE ELEMENT TO SI                   
         PUSH  USING                                                            
         USING SCIELD,BOELEM       ADD ZERO GROSS BUCKET TOO                    
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGRSS                                                 
         ZAP   SCIAMNT,BCPZERO                                                  
         POP   USING                                                            
         GOTO1 AADDXTRA                                                         
*                                  BUILD SKELETON MEDIA TRANSFER ELEM           
         GOTO1 AADDBMDT,BOPARM,BAPTAEL,BCJOBCOD                                 
         ZAP   BCDUB,POSTAMNT                                                   
         CVB   R0,BCDUB                                                         
         STCM  R0,15,BOELEM+(MDTCOM-MDTELD)                                     
         GOTO1 AADDXTRA                                                         
*                                                                               
         LA    RE,PBPTRS           RE=A(LIST OF ADDRESSES)                      
         CLC   BSICST,BCSPACES     TEST SI ACCOUNT ANALYSED                     
         BE    PIST08                                                           
         LA    RF,BCCMPPRF+(PPRCOSTU-PPRELD)                                    
         STCM  RF,15,0(RE)                                                      
         MVI   0(RE),APENSDR       1C POSTING IS A DEBIT                        
         LA    RE,4(RE)                                                         
         LA    RF,BSICST                                                        
         STCM  RF,15,0(RE)                                                      
         MVI   0(RE),0             12 POSTING IS A CREDIT                       
         LA    RE,4(RE)                                                         
PIST08   CLC   TLDUTPER,BCSPACES   TEST ANY 1R ACCOUNT                          
         BE    PIST10                                                           
         LA    RF,TLDUTPER                                                      
         STCM  RF,15,0(RE)                                                      
         MVI   0(RE),0             ?                                            
         LA    RE,4(RE)                                                         
PIST10   MVI   0(RE),FF            SET END                                      
*&&                                                                             
         CLI   CUCTRY,CTRYGER      TEST THIS IS GERMAN USER                     
         BNE   PIST12                                                           
         CLC   BVATAC,BCSPACES     CHECK THERE IS ONE                           
         BNH   PIST12                                                           
         LA    RF,BOELEM                                                        
         USING APEELD,RF                                                        
         MVI   APEEL,APEELQ        SET VAT ATTRIBUTE                            
         MVI   APENUM,1                                                         
         MVC   APENACT,BVATAC                                                   
         LA    R1,APENACT+L'APENACT-1                                           
         LA    RE,APELN1Q(RF)      GET LENGTH OF ACCOUNT                        
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         MVI   0(R1),0                                                          
         BCT   R1,*-12                                                          
         LA    R1,1(R1)                                                         
         SR    R1,RE                                                            
         STC   R1,APENLEN                                                       
         LA    R1,APELN1Q(R1)                                                   
         STC   R1,APELN                                                         
         DROP  RF                                                               
         GOTO1 AADDXTRA                                                         
PIST12   MVI   POSTSTAT,0          CREDIT SI                                    
         GOTO1 AADDOCA,BODMCB,('QTRNAMNT',TLDUTRFF),(X'FF',0)                   
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         MVI   PBPTRS,FF                                                        
*                                                                               
         CLC   BSICST,BCSPACES     TEST ANY ANALYSIS POSTINGS NEEDED            
         BE    PIST14                                                           
         MVC   POSTACT,BCCMPPRF+(PPRCOSTU-PPRELD)                               
         MVC   POSTCAC,BCSPACES                                                 
         MVC   POSTCAC(L'ACTKCPY),CUABIN                                        
         MVC   POSTCAC+L'ACTKCPY(L'ACTKULA),BSICST                              
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
         MVI   POSTSTAT,TRNSDR                                                  
         GOTO1 AADDOCA,BODMCB,('QTRNAMNT',TLDUTRFF),(X'FF',0)                   
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
*                                  CREDIT COSTING INCOME                        
         MVC   POSTCAC+1(14),POSTACT                                            
         MVC   POSTACT,BSICST                                                   
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
         MVI   POSTSTAT,0                                                       
         GOTO1 AADDOCA,BODMCB,('QTRNAMNT',TLDUTRFF),(X'FF',0)                   
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
*                                                                               
PIST14   B     PIST06              GET NEXT TRANSFER RECORD                     
*                                                                               
PSTISTX  B     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
*              BUILD 99 BILLING POSTING TO JOB                       *          
**********************************************************************          
         SPACE 1                                                                
PSTBIL   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         MVC   POSTACT(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         MVC   POSTACT+L'CPYPROD(L'BCJOBCOD),BCJOBCOD                           
         MVC   POSTACTN,BCJOBNAM                                                
         MVC   POSTCAC(L'ACTKCPY),CUABIN                                        
         MVC   POSTCAC+L'ACTKCPY(L'PBDEBULA),PBDEBULA                           
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
         MVC   POSTOFFC,=C'99'                                                  
         MVC   POSTDATE,PBDATP     TRANSACTION DATE IS BILL DATE                
         MVC   POSTREF,PBLIVE#     TRANSACTION REF IS LIVE BILL NUM             
         OC    POSTREF,POSTREF     OR DRAFT BILL NUM                            
         BNZ   *+10                                                             
         MVC   POSTREF,PBDRAFT#                                                 
         ZAP   POSTAMNT,PBCBAPN    TOTAL NET ALLOCATION                         
*&&UK*&& LH    RE,=Y(LC@CLIBG-TWAD)                                             
*&&US*&& LH    RE,=Y(UC@CLIBG-TWAD)                                             
         AR    RE,RA                                                            
         XC    POSTNARR,POSTNARR                                                
         MVC   POSTNARR(L'LC@CLIBG),0(RE)                                       
*                                                                               
*&&US                                                                           
         LA    RE,POSTNARR+15      SET BUCKETS FOR COMM/CDISC/PAYABLE           
         LA    R0,13                                                            
         ZAP   0(6,RE),BCPZERO                                                  
         LA    RE,6(RE)                                                         
         BCT   R0,*-10                                                          
         ZAP   POSTNARR+15(6),PBCBAPC COMMISSION BILLED                         
         ZAP   POSTNARR+21(6),PBCBAPD DISCOUNT BILLED                           
         ZAP   POSTNARR+27(6),PBCBAPN PAYABLE=(GROSS-DISCOUNT)                  
         AP    POSTNARR+27(6),PBCBAPC                                           
         SP    POSTNARR+27(6),PBCBAPD                                           
         MVI   POSTNARL,13*6+15    SET NARRATIVE LENGTH                         
*                                                                               
         LA    RF,BOELEM                                                        
         USING BSCELD,RF                                                        
         MVI   BSCEL,BSCELQ        ADD BILLING SOURCE ELEMENT                   
         MVI   BSCLN,BSCLNQ                                                     
         MVC   BSCBSRC,BMEDNM                                                   
         OC    BSCBSRC,BCSPACES                                                 
         MVC   BSCBOFF,CSOFFICE                                                 
         DROP  RF                                                               
*&&                                                                             
*                                                                               
         LA    R3,BOELEM           SAVE COMMISSION                              
         USING SCIELD,R3                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITCOMM                                                 
         ZAP   SCIAMNT,PBCBAPC                                                  
         CLC   CSBILCUR,CSCPYCUR   TEST FOREIGN BILLING                         
         BE    *+14                                                             
         MVI   SCILN,SCILN2Q       YES - SAVE BILLING AMOUNT TOO                
         ZAP   SCIADMN,PBCBALC                                                  
         GOTO1 AADDXTRA                                                         
         DROP  R3                                                               
*                                                                               
         TM    BCGLOB1,BCGLVAT                                                  
         BZ    PBIL16                                                           
         XC    BOCASCI,BOCASCI                                                  
         XC    TLKEY,TLKEY         GET VAT TYPE TSAR RECORD                     
         MVI   TLKSES,TLKUVATQ                                                  
         LA    R1,TSARDH                                                        
         B     *+8                                                              
PBIL02   LA    R1,TSANXT                                                        
         GOTO1 ATSARIO,(R1)                                                     
         BL    PBIL16                                                           
         CLI   TLKSES,TLKUVATQ                                                  
         BNE   PBIL16              END OF VAT RECORDS                           
*                                                                               
*&&UK                                                                           
         ZAP   BODUB3,BCPZERO      SET BODUB3 WITH DISCOUNT AMOUNT              
         CP    PBDSCPC,BCPZERO     TEST ANY DISCOUNT                            
         BE    PBIL04                                                           
         ZAP   BODUB1(2*L'BODUB1),TLDUVATA                                      
         MP    BODUB1(2*L'BODUB1),PBDSCPC                                       
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         ZAP   BODUB3,BODUB2                                                    
*                                                                               
PBIL04   CP    PBSRCPC,BCPZERO     TEST ANY SURCHARGE                           
         BE    PBIL06                                                           
         ZAP   BODUB1(2*L'BODUB1),TLDUVVSG                                      
         MP    BODUB1(2*L'BODUB1),PBSRCPC                                       
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         ZAP   TLDUVVSG,BODUB2     SET SURCHARGE AMOUNT                         
         AP    PBTOTAS,BODUB2      ADD TO SURCHARGE TOTAL                       
         CLI   P#VATSCH,C'Y'                                                    
         BNE   *+10                                                             
         AP    TLDUVATA,BODUB2     ADD SURCHARGE TO VATABLE                     
*                                                                               
PBIL06   AP    PBTOTAD,BODUB3      ADD TO DISCOUNT TOTAL                        
         SP    TLDUVATA,BODUB3     VATABLE AMOUNT IS DISCOUNTED                 
*                                                                               
         CP    PBDSCPC,BCPZERO     TEST ANY DISCOUNT                            
         BE    PBIL08                                                           
         ZAP   BODUB1(2*L'BODUB1),TLDUVTFA                                      
         MP    BODUB1(2*L'BODUB1),PBDSCPC                                       
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         SP    TLDUVTFA,BODUB2     VATABLE AMOUNT IS DISCOUNTED                 
*                                                                               
PBIL08   CP    PBSRCPC,BCPZERO     TEST ANY SURCHARGE                           
         BE    PBIL10                                                           
         CLI   P#VATSCH,C'Y'                                                    
         BNE   PBIL10                                                           
         ZAP   BODUB1(2*L'BODUB1),TLDUVFSG                                      
         MP    BODUB1(2*L'BODUB1),PBSRCPC                                       
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         AP    TLDUVTFA,BODUB2     ADD SURCHARGE TO VATABLE                     
*                                                                               
PBIL10   EQU   *                                                                
*&&                                                                             
*                                                                               
         PUSH  USING                                                            
         L     R1,AIO1                                                          
         USING VTCD,R1                                                          
         XC    VTCD(VTCLNQ),VTCD                                                
         MVI   VTCACTN,VTCALOOK                                                 
         MVC   VTCCPY,CUABIN                                                    
         MVC   VTCOFFC,CSOFFICE                                                 
         MVC   VTCTYPE,TLKUVATT                                                 
         MVC   VTCCOMF,ACOM                                                     
         MVC   VTCINVD,PBDATP                                                   
         OC    PBDATVP,PBDATVP                                                  
         BZ    *+10                                                             
         MVC   VTCINVD,PBDATVP                                                  
*                                  MAY BE OVERRIDE VAT DATE                     
         GOTO1 VVATICAN                                                         
         MVC   TLDUVRAT,VTCRATE                                                 
         OC    BVATAC,BVATAC       TEST ACCOUNT ALREADY SAVED                   
         BZ    *+14                NO - ALWAYS SAVE THE FIRST                   
         OC    TLDUVRAT,TLDUVRAT   TEST NON-ZERO VAT RATE                       
         BZ    *+10                                                             
         MVC   BVATAC,VTCACT+1     SAVE THE POSTING ACCOUNT                     
         MVC   TLDUVACT,VTCACT                                                  
         MVC   TLDUVIND,VTCINDS                                                 
         MVC   TLDUVANM,VTCACTNM                                                
         MVC   TLDUVTNM,VTCTTYPE                                                
         MVC   TLDUVEFF,VTCEFFD                                                 
         OC    TLDUVRAT,TLDUVRAT                                                
         BZ    PBIL12                                                           
         SR    RE,RE                                                            
         ICM   RE,3,TLDUVRAT                                                    
         ZAP   BODUB1(2*L'BODUB1),BCPZERO                                       
         CVD   RE,BODUB2                                                        
         MP    BODUB1(2*L'BODUB1),TLDUVATA                                      
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         ZAP   TLDUVAT,BODUB1(2*L'BODUB1)                                       
         AP    PBTOTAV,TLDUVAT     ADD VAT INTO BILL TOTAL                      
         ZAP   BODUB1(2*L'BODUB1),BCPZERO                                       
         CVD   RE,BODUB2                                                        
         MP    BODUB1(2*L'BODUB1),TLDUVTFA                                      
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         ZAP   TLDUVTF,BODUB1(2*L'BODUB1)                                       
         AP    PBTOTFV,TLDUVTF                                                  
PBIL12   LA    R0,TLDUVTMX         SAVE VAT RATE FOR INCOME BUCKETS             
         LA    RF,BVRATT                                                        
         CLI   0(RF),0                                                          
         BE    *+14                                                             
         LA    RF,L'BVRATT(RF)                                                  
         BCT   R0,*-12                                                          
         DC    H'0'                                                             
         MVC   0(L'VTCTYPE,RF),VTCTYPE                                          
         MVC   L'VTCTYPE(L'VTCRATE,RF),VTCRATE                                  
         POP   USING                                                            
PBIL14   LA    R3,BOELEM                                                        
*&&UK                                                                           
         GOTO1 ABVATOCA                                                         
         USING SCIELD,R3                                                        
         MVI   SCIEL,SCIELQ        USERS MUST USE NEW VAT RECORD                
         MVI   SCILN,SCILN2Q                                                    
         MVC   SCITYPE,TLKUVATT                                                 
         NI    SCITYPE,FF-X'40'                                                 
         ZAP   SCIGBIL,TLDUVATA                                                 
         ZAP   SCIVBIL,TLDUVAT                                                  
*&&                                                                             
*&&US                                                                           
         USING VBIELD,R3                                                        
         XC    VBIEL(VBILNQ),VBIEL                                              
         MVI   VBIEL,VBIELQ                                                     
         MVI   VBILN,VBILNQ                                                     
         MVC   VBITYPE,TLKUVATT                                                 
         MVC   VBIRATE,TLDUVRAT                                                 
         ZAP   VBIVAT,TLDUVAT                                                   
         ZAP   VBIGROSS,TLDUVATA                                                
         ZAP   VBICOMM,TLDUVCOM                                                 
         MVC   VBIACCT,TLDUVACT                                                 
         MVC   VBIDATE,TLDUVEFF                                                 
*&&                                                                             
         DROP  R3                                                               
*                                                                               
         GOTO1 AADDXTRA                                                         
         GOTO1 ATSARIO,TSAPUT      WRITE BACK UPDATED RECORD                    
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PBIL02              GET NEXT VAT RECORD                          
*                                                                               
PBIL16   EQU   *                                                                
*&&US                                                                           
         TM    BCGLOB1,BCGLPST                                                  
         BNO   PBIL26                                                           
         XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,TLKUPSTQ                                                  
         LA    R1,TSARDH                                                        
         B     *+8                                                              
PBIL18   LA    R1,TSANXT                                                        
         GOTO1 ATSARIO,(R1)                                                     
         BL    PBIL26                                                           
         CLI   TLKSES,TLKUPSTQ                                                  
         BNE   PBIL26                                                           
         LA    RF,TLDUGT1                                                       
         LA    R0,TLDUGTMX                                                      
PBIL20   LA    RE,BVRATT           GST RATES                                    
         CLC   0(L'VTCTYPE,RE),0(RF)                                            
         BE    *+12                                                             
         LA    RE,L'BVRATT(RE)                                                  
         B     *-14                                                             
         ZAP   BODUB1(2*L'BODUB1),BCPZERO                                       
         SR    R2,R2                                                            
         ICM   R2,3,L'VTCTYPE(RE)                                               
         CVD   R2,BODUB2                                                        
         BZ    PBIL22                                                           
         MP    BODUB1(2*L'BODUB1),1(L'TLDUGAM1,RF)                              
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
PBIL22   ZAP   1(L'TLDUGAM1,RF),BODUB2                                          
         AP    TLDUPSTA,BODUB2     ADD GST AMOUNT TO PSTABLE AMOUNT             
         LA    RF,TLDUGT2-TLDUGT1(RF)                                           
         BCT   R0,PBIL20                                                        
*                                                                               
         PUSH  USING                                                            
         L     R1,AIO1                                                          
         USING VTCD,R1                                                          
         XC    VTCD(VTCLNQ),VTCD                                                
         MVI   VTCACTN,VTCALOOK                                                 
         MVC   VTCCPY,CUABIN                                                    
         MVC   VTCOFFC,CSOFFICE                                                 
         MVC   VTCPRV,TLKUPSTP                                                  
         MVC   VTCTYPE,TLKUPSTT                                                 
         MVC   VTCCOMF,ACOM                                                     
         MVC   VTCINVD,PBDATP                                                   
         OC    PBDATVP,PBDATVP                                                  
         BZ    *+10                                                             
         MVC   VTCINVD,PBDATVP                                                  
*                                  MAY BE OVERRIDE VAT DATE                     
         GOTO1 VVATICAN                                                         
         MVC   TLDUPRAT,VTCRATE                                                 
         MVC   TLDUPACT,VTCACT                                                  
         MVC   TLDUPIND,VTCINDS                                                 
         MVC   TLDUPTNM,VTCTTYPE                                                
         MVC   TLDUPEFF,VTCEFFD                                                 
         OC    TLDUPRAT,TLDUPRAT                                                
         BZ    PBIL24                                                           
         SR    RE,RE                                                            
         ICM   RE,3,TLDUPRAT                                                    
         ZAP   BODUB1(2*L'BODUB1),BCPZERO                                       
         CVD   RE,BODUB2                                                        
         MP    BODUB1(2*L'BODUB1),TLDUPSTA                                      
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
         ZAP   TLDUPST,BODUB1(2*L'BODUB1)                                       
         AP    PBTOTAP,TLDUPST                                                  
         POP   USING                                                            
*                                                                               
PBIL24   LA    R3,BOELEM                                                        
         USING PBIELD,R3                                                        
         XC    PBIEL(PBILNQ),PBIEL                                              
         MVI   PBIEL,PBIELQ                                                     
         MVI   PBILN,PBILNQ                                                     
         MVC   PBITYPE,TLKUPSTT                                                 
         MVC   PBIRATE,TLDUPRAT                                                 
         MVC   PBIPRV,TLKUPSTP                                                  
         ZAP   PBIPST,TLDUPST                                                   
         ZAP   PBIGROSS,TLDUPSTA                                                
         ZAP   PBICOMM,TLDUPCOM                                                 
         MVC   PBIACCT,TLDUPACT                                                 
         MVC   PBIDATE,TLDUPEFF                                                 
         GOTO1 AADDXTRA                                                         
         DROP  R3                                                               
*                                                                               
         GOTO1 ATSARIO,TSAPUT      WRITE BACK UPDATED RECORD                    
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PBIL18              GET NEXT PST RECORD                          
*                                                                               
*&&                                                                             
PBIL26   CP    PBTOTAS,BCPZERO                                                  
         BE    PBIL30              NO SURCHARGE                                 
         CLC   PBSRCAN,BCSPACES    TEST SURCHARGE A/C IS ANALYSED               
         BNH   PBIL30                                                           
         XC    TLKEY,TLKEY         GET COSTING TSAR RECORD                      
         MVI   TLKSES,TLKUCSTQ                                                  
         MVC   TLKUCOST,PBSRCAN                                                 
         MVI   TLKUCTYP,TLKUCSRQ   SET SPECIAL TYPE FOR SURCHARGE               
         GOTO1 ATSARIO,TSARDH                                                   
         BE    PBIL28                                                           
         XC    TLKEY,TLKEY         BUILD NEW RECORD                             
         MVC   TLRLEN,=Y(TLUPDLNQ)                                              
         XC    TLDU,TLDU           CLEAR RECORD                                 
         MVI   TLKSES,TLKUCSTQ                                                  
         MVC   TLKUCOST,PBSRCAN                                                 
         MVI   TLKUCTYP,TLKUCSRQ                                                
         ZAP   TLDUCNET,BCPZERO    NO NET                                       
         ZAP   TLDUCCOM,PBTOTAS    SURCHARGE IS ALL REVENUE                     
         ZAP   TLDUCNEF,BCPZERO                                                 
         ZAP   TLDUCCOF,BCPZERO                                                 
         LA    R1,TSAADD           ADD NEW TSAR RECORD                          
         B     *+14                                                             
PBIL28   AP    TLDUCCOM,PBTOTAS    ADD SURCHARGE TO REVENUE                     
         LA    R1,TSAPUT           WRITE BACK UPDATED RECORD                    
         GOTO1 ATSARIO,(R1)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PBIL30   CLC   CSBILCUR,CSCPYCUR                                                
         BE    PBIL32                                                           
*        ZAP   BODUB1,PBTOTAV      GET BILL TOTAL VAT IN CURRENCY               
*        LA    R2,BOWORK1                                                       
*        USING EURKBLKD,R2                                                      
*        XC    0(EURKBLKL,R2),0(R2)                                             
*        MVC   EURKCUFR,CSCPYCUR                                                
*        MVC   EURKCUTO,CSBILCUR                                                
*        MVC   EURKRULE,CSEXCVAL                                                
*        GOTO1 VEUREKA,BODMCB,('APPLYQ',BOWORK1),BODUB1,BODUB1                  
*        DROP  R2                                                               
*        ZAP   PBTOTFV,BODUB1                                                   
         LA    R3,BOELEM           FOREIGN CURRENCY ELEMENT                     
         USING AFCELD,R3                                                        
         XC    AFCEL(AFCLNQ),AFCEL                                              
         MVI   AFCEL,AFCELQ                                                     
         MVI   AFCLN,AFCLNQ                                                     
         MVC   AFCCURR,CSBILCUR                                                 
         ZAP   AFCAMNT,PBTOTFN                                                  
         MVC   AFCX,CSEXCVAL                                                    
         GOTO1 AADDXTRA                                                         
*                                                                               
PBIL32   MVI   POSTSTAT,0          JOB POSTING IS CREDIT                        
         MVI   PBPTRS,FF           NO ANALYSIS POINTERS                         
         GOTO1 AADDOCA,BODMCB,('QTRNAMNT',PBTOTFN),                    *        
               ('QSCITCOMM',PBCBALC),(X'FF',0)                                  
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         XC    BOCASCI,BOCASCI                                                  
         L     RE,POSTXTRA         CLEAR EXTRA ELEMENT BLOCK                    
         MVI   0(RE),0                                                          
         MVC   POSTACT,BCSPACES                                                 
         MVC   POSTCAC,BCSPACES                                                 
         MVI   POSTNARL,0                                                       
*                                                                               
PSTBILX  B     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
*              BUILD INCOME POSTINGS                                 *          
**********************************************************************          
         SPACE 1                                                                
PSTINC   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         MVC   POSTOFFC,CSOFFICE                                                
         LH    RE,=Y(LC@BILC-TWAD)                                              
         AR    RE,RA                                                            
         XC    POSTNARR,POSTNARR                                                
         MVC   POSTNARR(L'LC@BILC),0(RE)                                        
         MVC   POSTNARR+(L'LC@BILC+1)(L'PBLIVE#),PBLIVE#                        
         XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,TLKUWCQ      READ INCOME TOTAL RECORDS                    
         LA    R1,TSARDH                                                        
         B     PINC04                                                           
*                                                                               
PINC02   LA    R1,TSANXT                                                        
PINC04   GOTO1 ATSARIO,(R1)                                                     
         BL    PINC10                                                           
         CLI   TLKSES,TLKUWCQ      TEST END OF THIS RECORD TYPE                 
         BNE   PINC10                                                           
         CLC   TLKUACT,POSTACT                                                  
         BE    *+14                SAME INCOME ACCOUNT                          
         CLC   POSTACT,BCSPACES    TEST THIS IS THE FIRST ONE                   
         BNE   PINC10                                                           
         MVC   POSTACT,TLKUACT                                                  
         MVC   BSANAL,TLDUANAL     SAVE COSTING CODE                            
         MVC   BSUL,ULREV          ENSURE UNIT LEDGER SET CORRECTLY             
*                                                                               
         AP    PBACCAC,TLDUACOM                                                 
         AP    PBACCFC,TLDUFCOM                                                 
         AP    PBACCAN,TLDUANET                                                 
         AP    PBACCFN,TLDUFNET                                                 
*                                                                               
PINC06   LA    R3,BOELEM           INCOME-BY-WORKCODE CASH ELEMENT              
         USING SCIELD,R3                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITANAL                                                 
         ZAP   SCIAMNT,TLDUANET                                                 
         ZAP   SCIADMN,TLDUACOM                                                 
         MVC   SCISUBTY,BCSPACES                                                
         MVC   SCISUBTY(L'TLKUWC),TLKUWC                                        
         GOTO1 AADDXTRA                                                         
*                                  ADD VAT TYPE BUCKETS                         
*&&US*&& B     PINC02                                                           
         TM    BCGLOB1,BCGLVAT                                                  
         BZ    PINC02                                                           
*&&UK                                                                           
         LA    R1,TLDUVT1                                                       
PINC08   CLI   0(R1),0                                                          
         BE    PINC02              END OF VAT TYPE CODES                        
         LA    R3,BOELEM                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVC   SCITYPE,0(R1)                                                    
         NI    SCITYPE,FF-X'40'                                                 
         ZAP   SCIAMNT,L'TLDUVT1(L'TLDUVAM1,R1)                                 
         GOTO1 AADDXTRA                                                         
         LA    R1,(TLDUVT2-TLDUVT1)(R1)                                         
         B     PINC08                                                           
*&&                                                                             
*                                                                               
PINC10   LA    R3,BOELEM           GROSS BUCKET                                 
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGRSS                                                 
         ZAP   SCIAMNT,PBACCAN                                                  
         AP    SCIAMNT,PBACCAC                                                  
         GOTO1 AADDXTRA                                                         
*                                  CALCULATE VAT IN SCIELS                      
         ZAP   BODUB4,BCPZERO                                                   
         TM    BCGLOB1,BCGLVAT                                                  
         BZ    PINC20                                                           
*&&US                                                                           
         GOTO1 AGSTPST             BUILD GST/PST SCIELS                         
*&&                                                                             
*&&UK                                                                           
         L     R3,POSTXTRA         R3=(ELEMENTS)                                
         SR    R0,R0                                                            
PINC12   CLI   SCIEL,0                                                          
         BE    PINC18                                                           
         CLI   SCIEL,SCIELQ                                                     
         BNE   PINC16                                                           
         TM    SCITYPE,X'40'                                                    
         BO    PINC16                                                           
         MVC   BCBYTE1,SCITYPE                                                  
         OI    BCBYTE1,X'40'                                                    
         LA    RE,BVRATT           RE=(TABLE OF RATES)                          
         CLC   BCBYTE1,0(RE)                                                    
         BE    *+12                                                             
         LA    RE,L'BVRATT(RE)                                                  
         B     *-14                                                             
         ZAP   BODUB1(2*L'BODUB1),BCPZERO                                       
         SR    R2,R2               CALCULATE VAT AMOUNT                         
         ICM   R2,3,L'VTCTYPE(RE)                                               
         CVD   R2,BODUB2                                                        
         BZ    PINC14                                                           
         MP    BODUB1(2*L'BODUB1),SCIAMNT                                       
         SRP   BODUB1(2*L'BODUB1),64-4,5                                        
PINC14   ZAP   SCIAMNT,BODUB2                                                   
         AP    BODUB4,SCIAMNT      SAVE TOTAL VAT FOR THIS ACCOUNT              
PINC16   IC    R0,SCILN                                                         
         AR    R3,R0                                                            
         B     PINC12                                                           
*                                                                               
PINC18   LA    R3,BOELEM           TOTAL VAT BUCKET                             
         MVI   SCITYPE,SCITIVAT    TOTAL VAT BUCKET                             
         ZAP   SCIAMNT,BODUB4                                                   
         GOTO1 AADDXTRA                                                         
         DROP  R3                                                               
         CLI   CUCTRY,CTRYGER      TETS GERMAN USER                             
         BNE   PINC20                                                           
         CLC   BVATAC,BCSPACES     TEST THERE IS ONE                            
         BNH   PINC20                                                           
         USING APEELD,R3                                                        
         MVI   APEEL,APEELQ        SET VAT ATTRIBUTE                            
         MVI   APENUM,1                                                         
         MVC   APENACT,BVATAC                                                   
         LA    R1,APENACT+L'APENACT-1                                           
         LA    RE,APELN1Q(R3)      GET LENGTH OF ACCOUNT                        
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         MVI   0(R1),0                                                          
         BCT   R1,*-12                                                          
         LA    R1,1(R1)                                                         
         SR    R1,RE                                                            
         STC   R1,APENLEN                                                       
         LA    R1,APELN1Q(R1)                                                   
         STC   R1,APELN                                                         
         DROP  R3                                                               
         GOTO1 AADDXTRA                                                         
*&&                                                                             
*                                                                               
PINC20   EQU   *                                                                
*&&US                                                                           
         GOTO1 AADDBMDT,BOPARM,(1,BAPTAEL),BCJOBCOD ADD MEDIA XFER EL           
         LA    RF,PBPTRS                                                        
         LA    RE,BCCMPPRF+(PPRCOSTU-PPRELD)                                    
         STCM  RE,15,0(RF)                                                      
         OI    0(RF),APENSDR                                                    
         LA    RF,4(RF)                                                         
         LA    RE,BSUL                                                          
         STCM  RE,15,0(RF)                                                      
         MVI   4(RF),FF                                                         
*&&                                                                             
*                                                                               
         ZAP   POSTAMNT,PBACCAC    INCOME AMOUNT                                
         MVC   POSTCAC(L'ACTKCPY),CUABIN                                        
         MVC   POSTCAC+L'CUABIN(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)             
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCPROCOD),BCPROCOD                  
         MVC   POSTCACN,BCPRONAM                                                
*&&US                                                                           
         OC    BSANAC,BSANAC                                                    
         BZ    *+16                                                             
         MVC   POSTCAC,BSANAC                                                   
         MVC   POSTCACN,BSANNM                                                  
*&&                                                                             
         MVI   POSTSTAT,0          CREDIT                                       
         ZAP   BODUB1,PBACCFN                                                   
         AP    BODUB1,PBACCFC                                                   
         ZAP   BSECCOST,PBACCFC                                                 
         GOTO1 AADDOCA,BODMCB,('QTRNAMNT',PBACCFC),                    *        
               ('QSCITGRSS',BODUB1),(X'FF',0)                                   
         GOTO1 APSTACCR                                                         
         BH    EXIT                                                             
         L     RE,POSTXTRA                                                      
         MVI   0(RE),0                                                          
*                                                                               
         AP    PBTOTAC,PBACCAC     ADD INCOME ACC TOTS INTO BILL TOTS           
         AP    PBTOTFC,PBACCFC                                                  
         AP    PBTOTAN,PBACCAN                                                  
*                                                                               
         L     R1,ATSABLK                                                       
         TM    TSERRS-TSARD(R1),TSEEOF       E-O-F                              
         BO    PSTINCX                                                          
         CLI   TLKSES,TLKUWCQ      WAS THIS THE LAST INCOME RECORD              
         BNE   PSTINCX                                                          
         ZAP   PBACCAC,TLDUACOM    START NEW INCOME ACCOUNT TOTALS              
         ZAP   PBACCFC,TLDUFCOM                                                 
         ZAP   PBACCAN,TLDUANET                                                 
         ZAP   PBACCFN,TLDUFNET                                                 
         MVC   POSTACT,TLKUACT                                                  
         B     PINC06                                                           
*                                                                               
PSTINCX  DS    0H                                                               
         B     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
*              BUILD DEBTORS POSTING                                 *          
**********************************************************************          
         SPACE 1                                                                
PSTDEB   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         MVC   POSTACT,PBDEBULA                                                 
         MVC   POSTCAC,BCSPACES                                                 
         MVC   POSTCAC+(ACTKACT-ACTRECD)(L'BMEDNM),BMEDNM                       
         MVC   POSTCACN,BCSPACES                                                
         GOTO1 ASETACTN,BOPARM,POSTACT,POSTACTN                                 
         CLC   BCCPYREC,PBDEBUL    OVERRIDE LEDGER HAS JOB AS CONTRA            
         BE    PDEB02                                                           
         MVC   POSTCCPY,CUABIN                                                  
         MVC   POSTCUL,BCCPYPRD                                                 
         MVC   POSTCACT,BCJOBCOD                                                
         MVC   POSTCACN,BCJOBNAM                                                
PDEB02   ZAP   POSTAMNT,PBTOTAN    NET                                          
         AP    POSTAMNT,PBTOTAC    COMMISSION                                   
         AP    POSTAMNT,PBTOTAV    VAT                                          
         AP    POSTAMNT,PBTOTAP    PST                                          
         AP    POSTAMNT,PBTOTAS    INCREASE DEBTORS BY SURCHARGE                
         MVI   POSTSTAT,TRNSDR                                                  
*                                                                               
*&&US                                                                           
         GOTO1 AADDBMDT,BOPARM,(1,BAPTAEL),BCJOBCOD ADD MEDIA XFER EL           
*                                                                               
         LA    R3,BOELEM                                                        
         USING OTHELD,R3                                                        
         MVI   OTHEL,OTHELQ                                                     
         MVI   OTHLN,OTHLN1Q                                                    
         MVC   OTHNUM(L'OTHNUM+L'OTHPROF),BCSPACES                              
         MVI   OTHPROF,C'J'                                                     
         MVC   OTHNUM,BCJOBCOD+3                                                
         GOTO1 AADDXTRA                                                         
*&&                                                                             
*                                                                               
*&&UK                                                                           
         CLC   CSBILCUR,CSCPYCUR                                                
         BE    PDEB08                                                           
         ZAP   BODUB1,PBTOTAD      GET DISCOUNT IN CURRENCY                     
         BZ    PDEB04                                                           
         LA    R2,BOWORK1                                                       
         USING EURKBLKD,R2                                                      
         XC    0(EURKBLKL,R2),0(R2)                                             
         MVC   EURKCUFR,CSCPYCUR                                                
         MVC   EURKCUTO,CSBILCUR                                                
         MVC   EURKRULE,CSEXCVAL                                                
         GOTO1 VEUREKA,BODMCB,('APPLYQ',BOWORK1),BODUB1,BODUB1                  
         DROP  R2                                                               
PDEB04   ZAP   PBTOTFD,BODUB1                                                   
         ZAP   BODUB1,PBTOTAS      GET SURCHARGE IN CURRENCY                    
         BZ    PDEB06                                                           
         LA    R2,BOWORK1                                                       
         USING EURKBLKD,R2                                                      
         XC    0(EURKBLKL,R2),0(R2)                                             
         MVC   EURKCUFR,CSCPYCUR                                                
         MVC   EURKCUTO,CSBILCUR                                                
         MVC   EURKRULE,CSEXCVAL                                                
         GOTO1 VEUREKA,BODMCB,('APPLYQ',BOWORK1),BODUB1,BODUB1                  
         DROP  R2                                                               
PDEB06   ZAP   PBTOTFS,BODUB1                                                   
         LA    R3,BOELEM           FOREIGN CURRENCY ELEMENT                     
         USING AFCELD,R3                                                        
         XC    AFCEL(AFCLNQ),AFCEL                                              
         MVI   AFCEL,AFCELQ                                                     
         MVI   AFCLN,AFCLNQ                                                     
         OI    AFCXSTAT,AFCXPRIM                                                
         MVC   AFCCURR,CSBILCUR                                                 
         ZAP   AFCAMNT,PBTOTFN     NET                                          
         AP    AFCAMNT,PBTOTFC     COMMISSION                                   
         AP    AFCAMNT,PBTOTFV     VAT                                          
         CLI   CUCTRY,CTRYUSA                                                   
         BNE   *+10                                                             
         SP    AFCAMNT,PBTOTFD     DISCOUNT                                     
         AP    AFCAMNT,PBTOTFS     SURCHARGE                                    
         MVC   AFCX,CSEXCVAL                                                    
         GOTO1 AADDXTRA                                                         
*&&                                                                             
PDEB08   EQU   *                                                                
*&&UK                                                                           
         TM    BCGLOB1,BCGLVAT     EUROPEAN STYLE VAT SCIEL                     
         BZ    PDEB10                                                           
         USING SCIELD,R3                                                        
         LA    R3,BOELEM                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITTTAX                                                 
         ZAP   SCIAMNT,PBTOTAV                                                  
         GOTO1 AADDXTRA                                                         
*                                                                               
         USING SORELD,R3                                                        
PDEB10   MVI   SOREL,SORELQ        BILLING SOURCE ELEMENT (JOB)                 
         MVI   SORLN,SORALNQ                                                    
         MVI   SORSYS,SORSACC                                                   
         MVC   SORAULA(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         MVC   SORAULA+L'CPYPROD(L'BCPROCOD),BCJOBCOD                           
         GOTO1 AADDXTRA                                                         
*&&                                                                             
         LA    R3,BOELEM                                                        
         USING SCIELD,R3                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITCDSC                                                 
         ZAP   SCIAMNT,PBTOTAD     DISCOUNT ONLY                                
         BZ    PDEB12              NO BUCKET FOR ZERO DISCOUNT                  
         MP    SCIAMNT,=P'-1'                                                   
*        ZAP   SCIAMNT,PBTOTAS     **** SHOULD THIS BE US ONLY ****             
*        SP    SCIAMNT,PBTOTAD     **** SHOULD THIS BE US ONLY ****             
         GOTO1 AADDXTRA                                                         
PDEB12   DS    0H                                                               
*                                                                               
*&&US                                                                           
         GOTO1 AGSTPST             BUILD GST/PST SCIELS                         
         LA    R3,BOELEM                                                        
         MVI   SCIEL,SCIELQ        INCOME SCIEL                                 
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITINCA                                                 
         ZAP   SCIAMNT,PBTOTAC                                                  
         GOTO1 AADDXTRA                                                         
         USING CPJELD,R3                                                        
         MVI   CPJEL,CPJELQ        CLI/PRO/JOB ELEMENT                          
         MVI   CPJLN,CPJLNQ                                                     
         MVI   CPJTYPE,CPJTJOB                                                  
         MVC   CPJCLI(CPJLNQ),BCSPACES                                          
         MVC   CPJCLI,BCCLICOD                                                  
         MVC   CPJPRO,BCPROCOD+6                                                
         MVC   CPJJOB,BCJOBCOD+9                                                
         GOTO1 AADDXTRA                                                         
*&&                                                                             
         MVI   PBPTRS,FF                                                        
         ZAP   BODUB1,PBTOTFN                                                   
         AP    BODUB1,PBTOTFC                                                   
         AP    BODUB1,PBTOTFV                                                   
         AP    BODUB1,PBTOTFS                                                   
         GOTO1 AADDOCA,BODMCB,('QTRNAMNT',BODUB1),                     *        
               ('QSCITTTAX',PBTOTFV),(X'FF',0)                                  
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         L     RE,POSTXTRA         CLEAR EXTRA ELEMENT BLOCK                    
         MVI   0(RE),0                                                          
         XC    POSTNARR,POSTNARR                                                
*                                                                               
PSTDEBX  DS    0H                                                               
         B     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
*              BUILD DISCOUNT AND SURCHARGE POSTINGS                 *          
**********************************************************************          
         SPACE 1                                                                
PSTDSC   DS    0H                                                               
         USING *,R8                                                             
*&&UK                                                                           
         CP    PBTOTAD,BCPZERO                                                  
         BE    PDSC02              NO DISCOUNT                                  
         CLI   CUCTRY,CTRYUSA                                                   
         BNE   PDSC02              DISCOUNT POSTING IN USA ONLY                 
         MVC   POSTACT,PBDSCULA                                                 
         GOTO1 ASETACTN,BOPARM,POSTACT,POSTACTN                                 
         MVC   POSTCAC(L'CUABIN),CUABIN                                         
         MVC   POSTCAC+L'CUABIN(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)             
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCPROCOD),BCPROCOD                  
         MVC   POSTCACN,BCPRONAM                                                
         ZAP   POSTAMNT,PBTOTAD                                                 
         MP    POSTAMNT,=P'-1'                                                  
         MVI   POSTSTAT,0          CREDIT                                       
         CLC   =C'SI',POSTACT                                                   
         BE    *+8                                                              
         MVI   POSTSTAT,TRNSDR     SET DEBIT IF NOT SI                          
         MVI   PBPTRS,FF                                                        
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         L     RE,POSTXTRA                                                      
         MVI   0(RE),0                                                          
*                                                                               
PDSC02   CP    PBTOTAS,BCPZERO                                                  
         BE    PSTDSCX             NO SURCHARGE                                 
         MVC   POSTACT,PBSRCULA                                                 
         GOTO1 ASETACTN,BOPARM,POSTACT,POSTACTN                                 
         MVC   POSTCAC(L'CUABIN),CUABIN                                         
         MVC   POSTCAC+L'CUABIN(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)             
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCPROCOD),BCPROCOD                  
         MVC   POSTCACN,BCPRONAM                                                
         ZAP   POSTAMNT,PBTOTAS                                                 
         ZAP   BODUB1,PBTOTFS      SECONDARY CURRENCY AMOUNT                    
         MVI   POSTSTAT,0          CREDIT                                       
         CLC   =C'SI',POSTACT                                                   
         BE    PDSC04                                                           
         CLC   =C'SQ',POSTACT                                                   
         BE    PDSC04                                                           
         MVI   POSTSTAT,TRNSDR     SET DEBIT IF NOT SI                          
         MP    POSTAMNT,=P'-1'                                                  
         MP    BODUB1,=P'-1'                                                    
PDSC04   MVI   PBPTRS,FF                                                        
         GOTO1 AADDOCA,BODMCB,('QTRNAMNT',BODUB1),(X'FF',0)                     
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         L     RE,POSTXTRA                                                      
         MVI   0(RE),0                                                          
*&&                                                                             
PSTDSCX  DS    0H                                                               
         B     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
*              BUILD VAT/GST POSTINGS                                *          
**********************************************************************          
         SPACE 1                                                                
PSTVAT   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
*        ZAP   PBVATT,BCPZERO                                                   
*        ZAP   PBVATTF,BCPZERO                                                  
         TM    BCGLOB1,BCGLVAT                                                  
         BZ    PSTVATX                                                          
         XC    TLKEY,TLKEY         GET VAT TYPE TSAR RECORD                     
         MVI   TLKSES,TLKUVATQ                                                  
         LA    R1,TSARDH                                                        
         B     *+8                                                              
PVAT02   LA    R1,TSANXT                                                        
         GOTO1 ATSARIO,(R1)                                                     
         BL    PSTVATX                                                          
         CLI   TLKSES,TLKUVATQ                                                  
         BNE   PSTVATX             END OF VAT RECORDS                           
         MVC   POSTACT,TLDUVACT+(ACTKUNT-ACTKEY)                                
         CLI   POSTACT,C' '        TEST HAVE VALID VAT ACCOUNT                  
         BH    PVAT04                                                           
         MVC   FVMSGNO,=AL2(AE$OVAND)                                           
         MVC   FVXTRA(L'TLKUVATT),TLKUVATT                                      
         OI    CSINDSG1,CSINDUNW   UNWIND ANY UPDATES                           
         B     EXIT                                                             
PVAT04   MVC   POSTACTN,TLDUVANM                                                
         MVC   POSTCAC(L'ACTKCPY),CUABIN                                        
         MVC   POSTCAC+L'CUABIN(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)             
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCPROCOD),BCPROCOD                  
         MVC   POSTCACN,BCPRONAM                                                
         ZAP   POSTAMNT,TLDUVAT    VAT AMOUNT                                   
*        AP    PBVATT,TLDUVAT      TOTAL VAT IN AGENCY CURRENCY                 
*        AP    PBVATTF,TLDUVTF     TOTAL VAT IN FOREIGN CURRENCY                
         MVI   POSTSTAT,0          CREDIT                                       
         LA    R3,BOELEM                                                        
         USING SCIELD,R3                                                        
         MVI   SCIEL,SCIELQ        USERS MUST USE NEW VAT RECORD                
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGLEV                                                 
         ZAP   SCIAMNT,TLDUVATA                                                 
         GOTO1 AADDXTRA                                                         
         DROP  R3                                                               
         ZAP   BODUB1,TLDUVTF                                                   
         ZAP   BODUB2,TLDUVTFA                                                  
         GOTO1 AADDOCA,BODMCB,('QTRNAMNT',BODUB1),                     *        
               ('QSCITGLEV',BODUB2),(X'FF',0)                                   
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BNH   PVAT06                                                           
         CLC   FVXTRA,BCSPACES                                                  
         BH    EXIT                                                             
         MVC   FVMSGNO,=AL2(AE$VRNDE)                                           
         B     EXIT                                                             
PVAT06   L     RE,POSTXTRA         CLEAR EXTRA ELEMENT BLOCK                    
         MVI   0(RE),0                                                          
         B     PVAT02              GET NEXT VAT RECORD                          
*                                                                               
PSTVATX  DS    0H                                                               
         B     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
*              BUILD PST POSTINGS                                    *          
**********************************************************************          
         SPACE 1                                                                
PSTPST   DS    0H                                                               
         USING *,R8                                                             
*&&US                                                                           
         TM    BCGLOB1,BCGLPST                                                  
         BZ    PSTPSTX                                                          
         XC    TLKEY,TLKEY         GET PST TYPE TSAR RECORD                     
         MVI   TLKSES,TLKUPSTQ                                                  
         LA    R1,TSARDH                                                        
         B     *+8                                                              
PPST02   LA    R1,TSANXT                                                        
         GOTO1 ATSARIO,(R1)                                                     
         BL    PSTPSTX                                                          
         CLI   TLKSES,TLKUPSTQ                                                  
         BNE   PSTPSTX             END OF PST RECORDS                           
         MVC   POSTACT,TLDUPACT+(ACTKUNT-ACTKEY)                                
*        MVC   POSTACTN,TLDUVANM                                                
         MVC   POSTCAC(L'ACTKCPY),CUABIN                                        
         MVC   POSTCAC+L'CUABIN(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)             
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCPROCOD),BCPROCOD                  
         MVC   POSTCACN,BCPRONAM                                                
         ZAP   POSTAMNT,TLDUPST    PST AMOUNT                                   
         MVI   POSTSTAT,0          CREDIT                                       
         LA    R3,BOELEM                                                        
         USING SCIELD,R3                                                        
         MVI   SCIEL,SCIELQ        USERS MUST USE NEW VAT RECORD                
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGLEV                                                 
         ZAP   SCIAMNT,TLDUPSTA                                                 
         DROP  R3                                                               
         GOTO1 AADDXTRA                                                         
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BNH   PPST04                                                           
         CLC   FVXTRA,BCSPACES                                                  
         BH    EXIT                                                             
         MVC   FVMSGNO,=AL2(AE$VRNDE)                                           
         B     EXIT                                                             
PPST04   L     RE,POSTXTRA         CLEAR EXTRA ELEMENT BLOCK                    
         MVI   0(RE),0                                                          
         B     PPST02              GET NEXT VAT RECORD                          
*&&                                                                             
PSTPSTX  DS    0H                                                               
         B     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
*              BUILD COSTING POSTINGS                                *          
**********************************************************************          
         SPACE 1                                                                
PSTCST   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         MVC   POSTCAC,BCCMPPRF+(PPRCOST-PPRELD)                                
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
         MVC   BCSTNM,POSTCACN     SAVE CLIENT COSTING NAME                     
         MVC   BBLMCS,PBBILMC      SAVE TRANSACTION MOA                         
         XC    TLKEY,TLKEY         GET COSTING TSAR RECORD                      
         MVI   TLKSES,TLKUCSTQ                                                  
         LA    R1,TSARDH                                                        
         B     *+8                                                              
PCST02   LA    R1,TSANXT                                                        
         GOTO1 ATSARIO,(R1)                                                     
         BL    PSTCSTX                                                          
         CLI   TLKSES,TLKUCSTQ     TEST END OF COSTING RECORDS                  
         BNE   PSTCSTX                                                          
X        USING ACTKULA,BOWORK1     GET COSTING BILLINGS ACCOUNT NAME            
         MVC   X.ACTKULA,BCSPACES                                               
         MVC   X.ACTKUNT(L'ULBIL),ULBIL                                         
         MVC   X.ACTKACT,TLKUCOST                                               
         GOTO1 ASETACTN,BOPARM,X.ACTKULA,BULBNM                                 
         MVC   X.ACTKUNT(L'ULREV),ULREV GET COSTING REVENUES A/C NAME           
         GOTO1 ASETACTN,BOPARM,X.ACTKULA,BULRNM                                 
         DROP  X                                                                
         SR    R3,R3                                                            
         CLI   TLKUCTYP,TLKUCSRQ   TEST SURCHARGE COSTING                       
         BNE   *+14                                                             
         MVC   POSTBTMC,PBBILMC    RESTORE BILL MOA                             
         B     PCST06              AND NO MONTHLY SPLITTING                     
         CLI   PBMNTHSN,0                                                       
         BE    PCST06                                                           
         LA    R3,PBMNTHS                                                       
*                                  DEBIT COSTING CLIENT                         
PCST04   CLI   0(R3),0                                                          
         BE    PCST24                                                           
PCST06   MVC   POSTACT,BCCMPPRF+(PPRCOSTU-PPRELD)                               
         MVC   POSTACTN,BCSTNM                                                  
         MVC   POSTCAC,BCSPACES                                                 
         MVC   POSTCAC(L'ACTKCPY),CUABIN                                        
         MVC   POSTCAC+L'ACTKCPY(ACTKACT-ACTKUNT),ULBIL                         
         MVC   POSTCAC+(ACTKACT-ACTKEY)(L'TLKUCOST),TLKUCOST                    
         MVC   POSTCACN,BULBNM                                                  
         ZAP   POSTAMNT,TLDUCCOM   COMMISSION                                   
         LTR   R3,R3                                                            
         BZ    PCST08                                                           
         ZAP   BPSTAS,POSTAMNT                                                  
         GOTO1 APSTTRNA                                                         
         CLC   BBLMCS,POSTBTMC                                                  
         BNE   *+10                                                             
PCST08   AP    POSTAMNT,TLDUCNET   NET ALL GOES IN MONTH 1                      
         ZAP   BODUB2,TLDUCCOF                                                  
         AP    BODUB2,TLDUCNEF                                                  
         LTR   R3,R3                                                            
         BZ    PCST10                                                           
         ZAP   BODUB1(2*L'BODUB1),TLDUCCOF                                      
         MP    BODUB1(2*L'BODUB1),PBMPCT-PBMNTHSD(L'PBMPCT,R3)                  
         SRP   BODUB1(2*L'BODUB1),64-6,5                                        
         CLC   BBLMCS,POSTBTMC     TEST MONTH ONE                               
         BNE   PCST10                                                           
         ZAP   BODUB1,TLDUCCOF     ADJUST COMMISSION                            
         GOTO1 APSTDIFF                                                         
         AP    BODUB2,TLDUCNEF     AND ADD ALL NET                              
PCST10   ZAP   BSECCOST,BODUB2                                                  
         GOTO1 AADDOCA,BODMCB,('QTRNAMNT',BSECCOST),(X'FF',0)                   
PCST12   MVI   POSTSTAT,TRNSDR                                                  
*                                  CREDIT COSTING BILLINGS                      
         CP    POSTAMNT,BCPZERO                                                 
         BE    PCST14                                                           
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         MVC   POSTCAC+1(14),POSTACT                                            
         MVC   POSTCACN,BCSTNM                                                  
         MVC   POSTACT,BCSPACES                                                 
         MVC   POSTACT(ACTKACT-ACTKUNT),ULBIL                                   
         MVC   POSTACT+(ACTKACT-ACTKUNT)(L'TLKUCOST),TLKUCOST                   
         MVC   POSTACTN,BULBNM                                                  
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
         MVI   POSTSTAT,0                                                       
         GOTO1 AADDOCA,BODMCB,('QTRNAMNT',BSECCOST),(X'FF',0)                   
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
*                                  DEBIT COSTING CLIENT                         
PCST14   MVC   POSTACT,BCCMPPRF+(PPRCOSTU-PPRELD)                               
         MVC   POSTACTN,BCSTNM                                                  
         MVC   POSTCAC,BCSPACES                                                 
         MVC   POSTCAC(L'ACTKCPY),CUABIN                                        
         MVC   POSTCAC+L'ACTKCPY(ACTKACT-ACTKUNT),ULREV                         
         MVC   POSTCAC+(ACTKACT-ACTKEY)(L'TLKUCOST),TLKUCOST                    
         MVC   POSTCACN,BULRNM                                                  
         ZAP   POSTAMNT,TLDUCCOM   COMMISSION                                   
         LTR   R3,R3                                                            
         BZ    PCST16                                                           
         ZAP   BPSTAS,POSTAMNT                                                  
         GOTO1 APSTTRNA                                                         
PCST16   MVI   POSTSTAT,TRNSDR                                                  
         ZAP   BODUB2,TLDUCCOF                                                  
         LTR   R3,R3                                                            
         BZ    PCST18                                                           
         ZAP   BODUB1(2*L'BODUB1),TLDUCCOF                                      
         MP    BODUB1(2*L'BODUB1),PBMPCT-PBMNTHSD(L'PBMPCT,R3)                  
         SRP   BODUB1(2*L'BODUB1),64-6,5                                        
         CLC   BBLMCS,POSTBTMC     TEST MONTH ONE                               
         BNE   PCST18                                                           
         ZAP   BODUB1,TLDUCCOF     ADJUST COMMISSION                            
         GOTO1 APSTDIFF                                                         
PCST18   ZAP   BSECCOST,BODUB2                                                  
         GOTO1 AADDOCA,BODMCB,('QTRNAMNT',BSECCOST),(X'FF',0)                   
*                                  CREDIT COSTING INCOME                        
PCST20   CP    POSTAMNT,BCPZERO                                                 
         BE    PCST22                                                           
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         MVC   POSTCAC+1(14),POSTACT                                            
         MVC   POSTCACN,BCSTNM                                                  
         MVC   POSTACT,BCSPACES                                                 
         MVC   POSTACT(ACTKACT-ACTKUNT),ULREV                                   
         MVC   POSTACT+(ACTKACT-ACTKUNT)(L'TLKUCOST),TLKUCOST                   
         MVC   POSTACTN,BULRNM                                                  
         MVI   POSTSTAT,0                                                       
         GOTO1 AADDOCA,BODMCB,('QTRNAMNT',BSECCOST),(X'FF',0)                   
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
PCST22   LTR   R3,R3                                                            
         BZ    PCST24                                                           
         LA    R3,PBMNTHSL(R3)                                                  
         B     PCST04                                                           
*                                                                               
PCST24   B     PCST02              GET NEXT COSTING RECORD                      
*                                                                               
PSTCSTX  DS    0H                                                               
         B     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
*              BUILD WRITE-UP POSTINGS                               *          
**********************************************************************          
         SPACE 1                                                                
PSTWUP   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         XC    TLKEY,TLKEY         GET WRITE-UP TSAR RECORDS                    
         MVI   TLKSES,TLKUWUPQ                                                  
         LA    R1,TSARDH                                                        
         B     *+8                                                              
PWUP02   LA    R1,TSANXT                                                        
         GOTO1 ATSARIO,(R1)                                                     
         BL    PSTWUPX                                                          
         CLI   TLKSES,TLKUWUPQ     TEST END OF WRITE-UP RECORDS                 
         BNE   PSTWUPX                                                          
         MVC   POSTACT,TLKUPERS    POST TO PERSON                               
         MVC   POSTCAC(L'CUABIN),CUABIN                                         
         MVC   POSTCAC+L'CUABIN(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)             
         MVC   POSTCAC+L'CUABIN+L'CPYPROD(L'BCJOBCOD),BCJOBCOD                  
         MVI   POSTSTAT,TRNSDR                                                  
         ZAP   POSTAMNT,BCPZERO                                                 
         PUSH  USING                                                            
         USING SCIELD,BOELEM                                                    
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGRSS                                                 
         ZAP   SCIAMNT,TLDUWAMT                                                 
         GOTO1 AADDXTRA                                                         
         MVI   SCITYPE,SCITSJHR                                                 
         ZAP   SCIAMNT,TLDUWHRS                                                 
         GOTO1 AADDXTRA                                                         
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         B     PWUP02                                                           
         POP   USING                                                            
*                                                                               
PSTWUPX  DS    0H                                                               
         B     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
*              MAKE ANY REVERSE ESTIMATED PRODUCTION POSTINGS        *          
**********************************************************************          
         SPACE 1                                                                
PSTREP   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         XC    TLKEY,TLKEY         GET REVERSE E.P. RECORD                      
         MVI   TLKSES,TLKUREPQ                                                  
         LA    R1,TSARDH                                                        
         B     *+8                                                              
PREP02   LA    R1,TSANXT                                                        
         GOTO1 ATSARIO,(R1)                                                     
         BL    PSTREPX                                                          
         CLI   TLKSES,TLKUREPQ     TEST END OF REVERSE E.P. RECORDS             
         BNE   PSTREPX                                                          
         MVI   POSTTYPE,48                                                      
         MVC   POSTACT(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         MVC   POSTACT+L'CPYPROD(L'BCJOBCOD),BCJOBCOD                           
         MVC   POSTACTN,BCJOBNAM                                                
         MVC   POSTCAC(L'ACTKCPY),CUABIN                                        
         MVC   POSTCAC+L'ACTKCPY(L'TLKURCON),TLKURCON                           
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
         MVC   POSTOFFC,TLKURWRK                                                
         MVC   POSTDATE,TLKURDAT                                                
         MVC   POSTREF,TLKURREF                                                 
         ZAP   POSTAMNT,TLDUREPA                                                
         MP    POSTAMNT,=P'-1'                                                  
         XC    POSTNARR,POSTNARR                                                
         MVC   POSTNARR(19),=C'**AUTO REVERSE E.P.'                             
         MVI   POSTNARL,19                                                      
         MVI   POSTSTAT,TRNSDR                                                  
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
*                                                                               
         MVC   POSTACT,TLKURCON                                                 
         MVC   POSTCAC+L'ACTKCPY(L'TLDURCRC),TLDURCRC                           
         GOTO1 ASETACTN,BOPARM,POSTCUL,POSTCACN                                 
         MVC   POSTOFFC,CSOFFICE                                                
         MVI   POSTSTAT,0                                                       
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         B     PREP02                                                           
*                                                                               
PSTREPX  DS    0H                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* READ AND UPDATE 99 PRIOR BILLS                                      *         
***********************************************************************         
         SPACE 1                                                                
UPDPRB   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         TM    BINDS,BIREVBL       DID THIS BILL INCLUDE REVERSALS              
         BNO   UPDPRBX                                                          
         CLI   PBMODE,PBLIVEQ      TEST LIVE                                    
         BNE   UPDPRBX                                                          
         LA    R2,IOKEY            READ AND UPDATE 99 PRIOR BILLS               
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,BCJOBCOD                                                 
         MVC   TRNKWORK,=C'99'                                                  
         LA    R1,IOHI+IOACCDIR+IO1                                             
         B     *+8                                                              
UPRB02   LA    R1,IOSQ+IOACCDIR+IO1                                             
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TRNKEY(TRNKCULC-TRNKEY),IOKEYSAV                                 
         BNE   UPDPRBX                                                          
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RATELD,RF                                                        
         SR    R0,R0                                                            
         L     RF,AIO1                                                          
         LA    RF,TRNRFST-TRNRECD(RF)                                           
UPRB04   IC    R0,RATLN                                                         
         AR    RF,R0                                                            
         CLI   RATEL,0                                                          
         BE    UPRB02                                                           
         CLI   RATEL,RATETAXQ                                                   
         BNE   UPRB04                                                           
         OC    RATRATE,RATRATE     TEST THIS BILL HAD ANY REVERSALS             
         BZ    UPRB02              IF NOT DON'T BOTHER TO WRITE BACK            
         NC    RATRATE,=X'8000'    CLEAR COUNT OF REVERSED ITEMS                
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    UPRB02                                                           
         DC    H'0'                                                             
*                                                                               
         DROP  RF                                                               
UPDPRBX  DS    0H                                                               
         B     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
*              UPDATE BILL HEADER RECORD (MAINFRAME)                 *          
**********************************************************************          
         SPACE 1                                                                
         PUSH  USING                                                            
HDRMNF   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         LA    R2,IOKEY                                                         
         USING PBRRECD,R2                                                       
         XC    PBRPAS,PBRPAS                                                    
         MVI   PBRPTYP,PBRPTYPQ                                                 
         MVC   PBRPCPY,CUABIN                                                   
         MVI   PBRPSUB,PBRPPASQ                                                 
         MVC   PBRPBLNO,PBDRAFT#                                                
         MVI   PBRPIND,PBRPIDFT                                                 
         GOTO1 AIO,IO1+IOHIUP+IOACCDIR                                          
         CLC   PBRPAS(PBRPUSER-PBRPAS),IOKEYSAV                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    PBRKSTAT,PBRSDELT   DELETE DRAFT PASSIVE                         
         CLI   PBMODE,PBLIVEQ                                                   
         BNE   HMNF02                                                           
         GOTO1 AIO,IO1+IOWRITE+IOACCDIR                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   PBRPIND,PBRPILVE    ADD LIVE PASSIVE                             
         MVC   PBRPBILD,BCTODAYC                                                
         MVC   PBRPBLNO,PBLIVE#                                                 
         MVC   PBRKBILD,PBRPBILD                                                
         NI    PBRKSTAT,FF-(PBRSDELT)                                           
         GOTO1 AIO,IO1+IOADD+IOACCDIR                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  GET MASTER RECORD                            
HMNF02   GOTO1 AIO,IO1+IOGETRUP+IOACCMST                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
*                                                                               
         LA    R3,PBRRFST                                                       
         USING BLHELD,R3                                                        
         XR    RF,RF                                                            
         CLI   BLHEL,BLHELQ                                                     
         BE    *+12                                                             
         IC    RF,BLHLN                                                         
         BXH   R3,RF,*-12                                                       
         ZAP   BLHDSC,PBDSCPC                                                   
         ZAP   BLHSCH,PBSRCPC                                                   
         CLI   PBMODE,PBLIVEQ      TEST IF ACTION DRAFT                         
         BNE   HMNF04                                                           
         MVC   BLHBILD,BCTODAYC    BILLED DATE                                  
         MVC   BLHTRND,PBDATC      BILL DATE                                    
         MVC   BLHBLNO,PBLIVE#     LIVE BILL NUMBER                             
         MVC   BLHDUED,PBDUE       DUE DATE                                     
         MVC   BLHBREF,PBBILREF    BATCH REFERENCE                              
         MVC   BLHBMOS,PBBILMP     BILL MOS                                     
         MVC   PBRRBILD,BCTODAYC   SET BILLED DATE IN STATUS AREA               
         DROP  R3                                                               
*                                                                               
HMNF04   GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('SCIELQ',AIO1),            *        
               (1,=AL1(SCITCBSG)),0                                             
         CLI   12(R1),0                                                         
         BNE   HMNF06                                                           
         L     RE,12(R1)                                                        
         ZAP   SCIAMNT-SCIELD(L'SCIAMNT,RE),PBTOTAS                             
         CLC   CSBILCUR,CSCPYCUR   TEST FOREIGN CURRENCY                        
         BE    *+10                                                             
         ZAP   SCIAMNT-SCIELD(L'SCIAMNT,RE),PBTOTFS                             
         B     HMNF08                                                           
*                                                                               
         PUSH  USING                                                            
         USING SCIELD,BOELEM                                                    
HMNF06   MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITCBSG    ACTUAL SURCHARGE AMOUNT                      
         ZAP   SCIAMNT,PBTOTAS                                                  
         CLC   CSBILCUR,CSCPYCUR   TEST FOREIGN CURRENCY                        
         BE    *+10                                                             
         ZAP   SCIAMNT,PBTOTFS                                                  
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),AIO1,BOELEM,0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
HMNF08   GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('SCIELQ',AIO1),            *        
               (1,=AL1(SCITCBDC)),0                                             
         CLI   12(R1),0                                                         
         BNE   HMNF10                                                           
         L     RE,12(R1)                                                        
         ZAP   SCIAMNT-SCIELD(L'SCIAMNT,RE),PBTOTAD                             
         CLC   CSBILCUR,CSCPYCUR   TEST FOREIGN CURRENCY                        
         BE    *+10                                                             
         ZAP   SCIAMNT-SCIELD(L'SCIAMNT,RE),PBTOTFD                             
         B     HMNF12                                                           
*                                                                               
HMNF10   MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITCBDC    ACTUAL DISCOUNT AMOUNT                       
         ZAP   SCIAMNT,PBTOTAD                                                  
         CLC   CSBILCUR,CSCPYCUR   TEST FOREIGN CURRENCY                        
         BE    *+10                                                             
         ZAP   SCIAMNT,PBTOTFD                                                  
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),AIO1,BOELEM,0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*                                                                               
HMNF12   GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   PBMODE,PBLIVEQ                                                   
         BNE   HDRMNFX                                                          
         MVC   IOKEY,0(R2)         GET PRIME DIRECTORY KEY                      
         GOTO1 AIO,IO1+IORDUP+IOACCDIR                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY                                                         
         MVC   PBRKBILD,BCTODAYC   SET BILL DATE IN PRIME KEY                   
         GOTO1 AIO,IO1+IOWRITE+IOACCDIR                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CSBILNUM,PBLIVE#    PASS LIVE NUMBER TO PRINT                    
*                                                                               
HDRMNFX  B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
**********************************************************************          
*              UPDATE BILL HEADER RECORD (PC BILLING)                *          
**********************************************************************          
         SPACE 1                                                                
         PUSH  USING                                                            
HDRPCB   DS    0H                  * PC BILLING *                               
         USING *,R8                                                             
*                                                                               
K        USING BEDRECD,IOKEY       K = ACCDIR RECORD                            
         L     R2,AIO1                                                          
R        USING BEDRECD,R2          R = ACCMST RECORD                            
*                                                                               
         XC    K.BEDPAS,K.BEDPAS                                                
         MVI   K.BEDPTYP,BEDPTYPQ                                               
         MVC   K.BEDPCPY,CUABIN                                                 
         MVI   K.BEDPSUB,BEDPSUBQ                                               
         MVC   K.BEDPBLNO,PBDRAFT#                                              
         GOTO1 AIO,IO1+IOHIUP+IOACCDIR                                          
         CLC   K.BEDPAS(BEDPIND-BEDPAS),IOKEYSAV                                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IO1+IOGETRUP+IOACCMST                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AGETSCI,BOPARM,R.BEDRECD,SCITCBSG,SCILN1Q                        
         L     R3,12(R1)                                                        
         USING SCIELD,R3           SAVE SURCHARGE AMOUNT                        
         ZAP   SCIAMNT,PBTOTAS                                                  
         CLC   CSBILCUR,CSCPYCUR   TEST FOREIGN CURRENCY                        
         BE    *+10                                                             
         ZAP   SCIAMNT,PBTOTFS                                                  
*                                                                               
         GOTO1 AGETSCI,BOPARM,R.BEDRECD,SCITCBDC,SCILN1Q                        
         L     R3,12(R1)           SAVE DISCOUNT AMOUNT                         
         ZAP   SCIAMNT,PBTOTAD                                                  
         CLC   CSBILCUR,CSCPYCUR   TEST FOREIGN CURRENCY                        
         BE    *+10                                                             
         ZAP   SCIAMNT,PBTOTFD                                                  
*                                                                               
         GOTO1 AGETSCI,BOPARM,R.BEDRECD,SCITTTAX,SCILN2Q                        
         L     R3,12(R1)           SAVE VAT AMOUNT                              
         ZAP   SCIAMNT,PBTOTAV     AGENCY AMOUNT                                
         ZAP   SCIADMN,PBTOTFV     BILLING AMOUNT                               
         CLC   CSBILCUR,CSCPYCUR                                                
         BNE   *+10                                                             
         ZAP   SCIADMN,PBTOTAV     AGENCY IS BILLING AMOUNT                     
         DROP  R3                                                               
*                                                                               
         LA    R3,R.BEDRFST                                                     
         USING BLHELD,R3           SEARCH FOR BLHELD ELEMENT                    
         XR    RF,RF                                                            
         CLI   BLHEL,BLHELQ                                                     
         BE    *+12                                                             
         IC    RF,BLHLN                                                         
         BXH   R3,RF,*-12                                                       
*                                                                               
         CLI   PBMODE,PBLIVEQ      TEST IF ACTION LIVE                          
         BNE   HPCB02                                                           
         TM    BLHINDS1,BLHIREP    OR REPLICATE BILL                            
         BO    HPCB02                                                           
         ZAP   BLHDSC,PBDSCPC      DISCOUNT PERCENT                             
         ZAP   BLHSCH,PBSRCPC      SURCHARGE PERCENT                            
         MVC   BLHTRND,PBDATC      BILL DATE                                    
         MVC   BLHDUED,PBDUE       DUE DATE                                     
         MVC   BLHBMOS,PBBILMP     BILL MOS                                     
         MVC   BLHBREF,PBBILREF    BATCH REFERENCE                              
         MVC   BLHBILD,BCTODAYC    BILLED DATE                                  
         MVC   BLHBLNO,PBLIVE#     LIVE BILL NUMBER                             
         MVC   R.BEDRBILD,BCTODAYC SET BILLED DATE IN STATUS AREA               
*                                                                               
HPCB02   DS    0H                  UPDATE BILL HEADER RECORD                    
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,ALINK            UPDATE SAVED BLHED                           
         USING LINKD,RF                                                         
         XC    BEWBLH,BEWBLH                                                    
         IC    RE,BLHLN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   BEWBLH(0),BLHELD                                                 
         DROP  RF                                                               
*                                                                               
         CLI   PBMODE,PBLIVEQ      FINISHED IF NOT MAKING BILL LIVE             
         BNE   HDRPCBX                                                          
         MVC   CSBILNUM,PBLIVE#    PASS LIVE NUMBER TO PRINT                    
         TM    BLHINDS1,BLHIREP    TEST REPLICATE BILL                          
         BO    HPCB10                                                           
         OI    K.BEDKSTAT,BEDSDELT DELETE DRAFT PASSIVE                         
         GOTO1 AIO,IO1+IOWRITE+IOACCDIR                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   K.BEDPIND,BEDPILVE  ADD LIVE PASSIVE                             
         MVC   K.BEDPBILD,BCTODAYC                                              
         MVC   K.BEDPBLNO,PBLIVE#                                               
         MVC   K.BEDKBILD,K.BEDPBILD                                            
         NI    K.BEDKSTAT,FF-(BEDSDELT)                                         
         GOTO1 AIO,IO1+IOADD+IOACCDIR                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   K.BEDKEY,R.BEDKEY   GET PRIME DIRECTORY KEY                      
         GOTO1 AIO,IO1+IORDUP+IOACCDIR                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   K.BEDKBILD,BCTODAYC SET BILL DATE IN PRIME KEY                   
         GOTO1 AIO,IO1+IOWRITE+IOACCDIR                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     HDRPCBX                                                          
*                                                                               
HPCB10   DS    0H                  REPLICATE BILL MADE LIVE                     
         ZAP   BLHDSC,PBDSCPC      DISCOUNT PERCENT                             
         ZAP   BLHSCH,PBSRCPC      SURCHARGE PERCENT                            
         MVC   BLHTRND,PBDATC      BILL DATE                                    
         MVC   BLHDUED,PBDUE       DUE DATE                                     
         MVC   BLHBMOS,PBBILMP     BILL MOS                                     
         MVC   BLHBREF,PBBILREF    BATCH REFERENCE                              
         MVC   BLHBILD,BCTODAYC    BILLED DATE                                  
         MVC   BLHBLNO,PBLIVE#     LIVE BILL NUMBER                             
         MVC   R.BEDRBILD,BCTODAYC SET BILLED DATE IN STATUS AREA               
         NI    BLHINDS1,FF-BLHIREP NEW LIVE BILL IS NOT A REPLICATE             
*                                                                               
         MVC   OLDSEQ,R.BEDKJSEQ   SAVE OLD BEDKJSEQ                            
         XC    K.BEDKEY,K.BEDKEY                                                
         MVI   K.BEDKTYP,BEDKTYPQ                                               
         MVC   K.BEDKCPY,CUABIN                                                 
         MVI   K.BEDKSUB,BEDKSUBQ                                               
         MVC   K.BEDKJOB,BCJOBCOD                                               
*                                                                               
         GOTO1 AIO,IOHIGH+IORDEL+IOACCDIR                                       
         CLC   K.BEDKEY(BEDKJSEQ-BEDKEY),IOKEYSAV                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,K.BEDKJSEQ                                                  
         BCTR  RE,0                                                             
         STCM  RE,3,NEWSEQ         STORE NEW SEQUENCE                           
         LTR   RE,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   R.BEDKJSEQ,NEWSEQ   MOVE IN NEW SEQUENCE                         
         GOTO1 AIO,IO1+IOADD+IOACCMST                                           
         BE    *+6                 ADD BILL HEADER RECORD                       
         DC    H'0'                                                             
         L     RF,ALINK                                                         
         USING LINKD,RF            SAVE REPLICATE BILL INFO                     
         OI    BEWINDS,BEWIUREP       FOR OTHER OVERLAYS                        
         MVC   BEWREPDA,IODA                                                    
         MVC   BEWREPKY,R.BEDKEY                                                
         DROP  RF                                                               
*                                                                               
         XC    K.BEDPAS,K.BEDPAS   BUILD PASSIVE KEY                            
         MVI   K.BEDPTYP,BEDPTYPQ                                               
         MVC   K.BEDPCPY,CUABIN                                                 
         MVI   K.BEDPSUB,BEDPSUBQ                                               
         MVC   K.BEDPBLNO,PBLIVE#                                               
         MVI   K.BEDPIND,BEDPILVE                                               
         MVC   K.BEDPUSER,BLHUSER                                               
         MVC   K.BEDPJOB,BLHJOB                                                 
         MVC   K.BEDPCRED,BLHCRED                                               
         MVC   K.BEDPBILD,BLHBILD                                               
         MVC   K.BEDPFORM,BLHFORM                                               
         MVC   K.BEDPPERS,BLHPERS                                               
         MVC   K.BEDKEXPD,BLHEXPD                                               
         MVC   K.BEDKBILD,BLHBILD                                               
         MVC   K.BEDKDA,IODA                                                    
         GOTO1 AIO,IOADD+IOACCDIR     ADD PASSIVE                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    K.BEDKEY,K.BEDKEY       READ THROUGH ALL OTHER BEDRECS           
         MVI   K.BEDKTYP,BEDKTYPQ      WITH OLD SEQUENCE                        
         MVC   K.BEDKCPY,CUABIN                                                 
         MVI   K.BEDKSUB,BEDKSUBQ                                               
         MVC   K.BEDKJOB,BLHJOB                                                 
         MVC   K.BEDKJSEQ,OLDSEQ                                                
         DROP  R3                                                               
*                                                                               
HPCB12   GOTO1 AIO,IO1+IOREAD+IOACCDIR                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IO1+IOSEQ+IOACCDIR                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   K.BEDKEY(BEDKLVL-BEDKEY),IOKEYSAV                                
         BNE   HDRPCBX                                                          
*                                                                               
         GOTO1 AIO,IO1+IOGET+IOACCMST                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   R.BEDKJSEQ,NEWSEQ   ADD NEW RECORD WITH NEW SEQUENCE             
         GOTO1 AIO,IO1+IOADD+IOACCMST                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         B     HPCB12                                                           
*                                                                               
HDRPCBX  B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
**********************************************************************          
*              INCOME POSTINGS COME THROUGH HERE FOR ACCRUING        *          
**********************************************************************          
         SPACE 1                                                                
PSTACCR  DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         CLI   PBMNTHSN,0                                                       
         BNE   PSTA020                                                          
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         B     PSTAX               NO MONTHLY SPREADING AROUND                  
*                                                                               
PSTA020  MVC   BPSTAC,POSTACT      SAVE POSTING ACCOUNT                         
         ZAP   BPSTAS,POSTAMNT     SAVE TRANSACTION AMOUNT                      
*                                                                               
         LA    R0,BXTRAS           COPY EXTRA ELEMENT AREA INTO SAVE            
         LA    RE,PBXTRA                                                        
         LH    R1,=Y(L'BXTRAS)                                                  
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING PBMNTHSD,R3                                                      
         LA    R3,PBMNTHS                                                       
PSTA050  CLI   PBMNTHSD,0          TEST END OF MONTH LIST                       
         BE    PSTA500             FINISHED                                     
         CLC   PBMMC,PBBILMC       TEST FIRST MONTH POSTING                     
         BNE   PSTA060                                                          
         ZAP   POSTAMNT,BCPZERO                                                 
         CLI   PBMPCT,X'FF'        MEANS USING ACTUAL AMOUNT                    
         BE    PSTA051                                                          
         CP    PBMPCT,=P'9999999'  TEST NOTHING IN MONTH 1                      
         BNE   PSTA052                                                          
         ZAP   PBMPCT,=P'0'        TEST NOTHING IN MONTH 1                      
         CLI   PBMPCT+PBMNTHSL,X'FF'                                            
         BNE   PSTA052                                                          
PSTA051  OI    BINDS,BIABAMT                                                    
         CP    PBMAMT,BCPZERO                                                   
         BE    PSTA054                                                          
         B     PSTA060                                                          
PSTA052  CP    PBMPCT,BCPZERO      TEST ZERO TO MONTH 1                         
         BNE   PSTA060                                                          
PSTA054  LA    RF,PBXTRA           ENSURE OCAEL AMOUNT IS ZERO TOO              
         SR    RE,RE                                                            
PSTA056  CLI   0(RF),0                                                          
         BE    PSTA300                                                          
         CLI   0(RF),OCAELQ                                                     
         BE    PSTA058                                                          
         IC    RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     PSTA056                                                          
PSTA058  CLI   OCANTYPE-OCAELD(RF),QTRNAMNT                                     
         BNE   PSTA300                                                          
         ZAP   OCANCASH-OCAELD(,RF),BCPZERO                                     
         B     PSTA300                                                          
PSTA060  GOTO1 APSTTRNA                                                         
         AP    PBMAMT,POSTAMNT     ADD TO THIS MONTH AMOUNT                     
         LA    RF,BXTRAS                                                        
         SR    RE,RE                                                            
         LA    R2,PBXTRA                                                        
PSTA100  MVI   0(R2),0                                                          
         CLI   0(RF),0                                                          
         BE    PSTA300             NO MORE ELEMENTS                             
         SR    RE,RE                                                            
         IC    RE,1(RF)                                                         
PSTA120  EX    RE,*+4                                                           
         MVC   0(0,R2),0(RF)       MOVE ELEMENT AND E-O-R MARKER                
         USING SCIELD,R2           APPLY MONTHLY PERCENTAGE                     
         CLI   SCIEL,OCAELQ        TEST OTHER CURRENCY ELEMENT                  
         BE    PSTA150                                                          
         CLI   SCIEL,SCIELQ                                                     
         BNE   PSTA200                                                          
         CLI   SCITYPE,SCITIVAT    TEST IF VAT BUCKET                           
         BE    PSTA130                                                          
         CLI   SCITYPE,SCITGRSS    TEST IF GROSS BUCKET                         
         BE    PSTA130                                                          
         CLI   SCITYPE,SCITANAL    TEST IF INCOME ANALYSIS BUCKET               
         BE    PSTA130                                                          
         TM    SCITYPE,X'40'       TEST IF NEWVAT BUCKET                        
         BO    PSTA140                                                          
*                                                                               
PSTA130  CLC   POSTBTMC,PBBILMC    TEST FIRST MONTH POSTING                     
         BE    PSTA200             YES - BUCKET IS FULL AMOUNT                  
         ZAP   SCIAMNT,BCPZERO     ELSE SET AMOUNT TO ZERO                      
         CLI   SCILN,SCILN1Q                                                    
         BE    PSTA200                                                          
         ZAP   SCINET,BCPZERO                                                   
         B     PSTA200                                                          
*                                                                               
PSTA140  ZAP   BODUB1(2*L'BODUB1),SCIAMNT                                       
         MP    BODUB1(2*L'BODUB1),PBMPCT                                        
         SRP   BODUB1(2*L'BODUB1),64-6,5                                        
         ZAP   BODUB1,SCIAMNT      SAVE ORIGINAL (TOTAL)                        
         ZAP   SCIAMNT,BODUB2      SET THIS MONTH CALCULATED                    
         CLC   POSTBTMC,PBBILMC    TEST FIRST MONTH POSTING                     
         BNE   PSTA142                                                          
         LR    R0,RF                                                            
         GOTO1 APSTDIFF                                                         
         LR    RF,R0                                                            
         ZAP   SCIAMNT,BODUB2                                                   
PSTA142  CLI   SCILN,SCILN1Q                                                    
         BE    PSTA200                                                          
         ZAP   BODUB1(2*L'BODUB1),SCINET                                        
         MP    BODUB1(2*L'BODUB1),PBMPCT                                        
         SRP   BODUB1(2*L'BODUB1),64-6,5                                        
         ZAP   BODUB1,SCINET       SAVE ORIGINAL (TOTAL)                        
         ZAP   SCINET,BODUB2       SET THIS MONTH CALCULATED                    
         CLC   POSTBTMC,PBBILMC    TEST FIRST MONTH POSTING                     
         BNE   PSTA200                                                          
         LR    R0,RF                                                            
         GOTO1 APSTDIFF                                                         
         LR    RF,R0                                                            
         ZAP   SCINET,BODUB2       SET ADJUSTED MONTH 1 AMOUNT                  
         B     PSTA200                                                          
*                                                                               
         USING OCAELD,R2                                                        
PSTA150  ZAP   BODUB1(2*L'BODUB1),OCANCASH-OCAELD(L'OCANCASH,RF)                
         MP    BODUB1(2*L'BODUB1),PBMPCT                                        
         SRP   BODUB1(2*L'BODUB1),64-6,5                                        
         ZAP   BODUB1,OCANCASH-OCAELD(L'OCANCASH,RF)                            
         ZAP   OCANCASH,BODUB2     SET THIS MONTH CALCULATED                    
         ZAP   BSECCOST,OCANCASH   SAVE MONTH ONE AMOUNT FOR SQ                 
         CLC   POSTBTMC,PBBILMC    TEST FIRST MONTH POSTING                     
         BNE   PSTA152                                                          
         LR    R0,RF               ADJUST TRNAMNT, LEAVE FULL SCITGRSS          
         GOTO1 APSTDIFF                                                         
         LR    RF,R0                                                            
         ZAP   OCANCASH,BODUB2     SET ADJUSTED TRNAMNT                         
         ZAP   BSECCOST,OCANCASH   SAVE MONTH ONE AMOUNT FOR SQ                 
         B     PSTA200                                                          
PSTA152  CLI   OCAINDS,2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,OCANTRY+OCANTRYL ZEROISE SCITGRSS                             
         CLI   OCANTYPE-OCANTRY(RE),QSCITGRSS                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   OCANCASH-OCANTRY(L'OCANCASH,RE),BCPZERO                          
         DROP  R2                                                               
*                                                                               
PSTA200  SR    RE,RE               RESET INCREMENT VALUE                        
         IC    RE,1(RF)                                                         
         AR    RF,RE               MOVE 'FROM' POINTER                          
         AR    R2,RE               MOVE 'TO' POINTER                            
         B     PSTA100                                                          
*                                                                               
PSTA300  MVI   PBPTRS,FF                                                        
         MVI   POSTSTAT,0                                                       
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
PSTA350  MVC   POSTACT,PBACCULA    SET ACCRUAL ACCOUNT                          
         CLC   POSTBTMC,PBBILMC    TEST FIRST MONTH POSTING                     
         BE    PSTA352                                                          
         MVI   POSTSTAT,TRNSDR                                                  
         GOTO1 AADDOCA,BODMCB,('QTRNAMNT',BSECCOST),(X'FF',0)                   
         B     PSTA400                                                          
PSTA352  ZAP   BODUB1,POSTAMNT     MONTH 1 SQ POSTING IS CREDIT                 
         ZAP   POSTAMNT,BPSTAS                                                  
         SP    POSTAMNT,BODUB1                                                  
         LA    RF,BXTRAS                                                        
         SR    RE,RE                                                            
PSTA354  CLI   0(RF),0                                                          
         BE    PSTA400                                                          
         CLI   0(RF),OCAELQ                                                     
         BE    PSTA356                                                          
         IC    RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     PSTA354                                                          
PSTA356  ZAP   BODUB1,OCANCASH-OCAELD(,RF)                                      
         SP    BODUB1,BSECCOST                                                  
         GOTO1 AADDOCA,BODMCB,('QTRNAMNT',BODUB1),(X'FF',0)                     
PSTA400  GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BH    EXIT                                                             
         MVC   POSTACT,BPSTAC      RESET SAVED ACCOUNT                          
         LA    R3,PBMNTHSL(R3)     NEXT MONTH                                   
         B     PSTA050                                                          
PSTA500  MVC   POSTBTMC,PBBILMC    RESTORE BILL MOA                             
         MVC   POSTBTRF,PBBILREF   RESTORE BILL REFERENCE                       
PSTAX    DS    0H                                                               
         B     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
*              FIX POSTAMNT                                          *          
**********************************************************************          
         SPACE 1                                                                
PSTTRNA  DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         MVC   POSTBTMC,PBMMC      SET THIS MOS                                 
         MVC   POSTBTRF,PBMREF     SET THIS REFERENCE                           
         ZAP   BODUB2,BCPZERO      SET POSTING AMOUNT TO ZERO                   
         CLI   PBMPCT,X'FF'                                                     
         BNE   PTRN10                                                           
         ZAP   BODUB1(2*L'BODUB1),PBMAMT                                        
         MP    BODUB1(2*L'BODUB1),=P'1000000'                                   
         DP    BODUB1(16),PBBHAPC                                               
         ZAP   PBMPCT,BODUB1                                                    
*        ZAP   BODUB2,PBMAMT                                                    
*        B     PTRN20                                                           
*                                                                               
PTRN10   CP    PBMPCT,BCPZERO      TEST ZERO PERCENT FOR THIS MONTH             
         BE    PTRN20                                                           
         ZAP   BODUB1(2*L'BODUB1),BPSTAS                                        
         MP    BODUB1(2*L'BODUB1),PBMPCT                                        
         SRP   BODUB1(2*L'BODUB1),64-6,5                                        
         CLC   POSTBTMC,PBBILMC    TEST FIRST MONTH POSTING                     
         BNE   PTRN20                                                           
         ZAP   BODUB1,BPSTAS                                                    
         GOTO1 APSTDIFF                                                         
PTRN20   ZAP   POSTAMNT,BODUB2     SET TRANSACTION AMOUNT                       
         B     EXITY                                                            
         SPACE 1                                                                
**********************************************************************          
*              POST PENNY DIFFERENCES TO MONTH 1                     *          
**********************************************************************          
         SPACE 1                                                                
PSTDIFF  DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         TM    BINDS,BIABAMT       TEST ACCRUALS ABSOLUTE AMOUNTS               
         BO    PSTDX                                                            
         ZAP   BTARGT,BODUB1       TOTAL TO AIM FOR                             
         ZAP   BMNTH1,BODUB2       MONTH 1 CALCULATED AMOUNT                    
         ZAP   BODUB3,BCPZERO      ROLLING TOTAL                                
PSTD100  CLI   PBMNTHSD,0                                                       
         BE    PSTD200                                                          
         ZAP   BODUB1(2*L'BODUB1),BTARGT                                        
         MP    BODUB1(2*L'BODUB1),PBMPCT                                        
         SRP   BODUB1(2*L'BODUB1),64-6,5                                        
         AP    BODUB3,BODUB2                                                    
         LA    R3,PBMNTHSL(R3)                                                  
         B     PSTD100                                                          
PSTD200  SP    BODUB3,BTARGT       TAKE OFF TARGET                              
         SP    BMNTH1,BODUB3       ADJUST MONTH 1 AMOUNT                        
         ZAP   BODUB2,BMNTH1       RETURN ADJUSTED AMOUNT                       
PSTDX    B     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
*              ADD OCAEL                                             *          
**********************************************************************          
         SPACE 1                                                                
ADDOCA   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         CLC   CSBILCUR,BCCPYSEC   TEST SECOND CURRENCY BILLING                 
         BNE   EXITY                                                            
         LA    R3,BOELEM                                                        
         USING OCAELD,R3                                                        
         CLI   0(R1),X'FF'         HAVE WE BEEN PASSED ANY ELEMENTS?            
         BE    ADDOCAX                                                          
         MVI   OCAEL,OCAELQ                                                     
         MVI   OCAINDS,0                                                        
         SR    RE,RE                                                            
         LA    R2,OCANTRY                                                       
         USING OCANTRY,R2                                                       
ADDOC02  L     RF,0(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BE    ADDOC04                                                          
         MVI   OCANSEQN,1                                                       
         MVC   OCANTYPE,0(R1)                                                   
         CLI   OCANTYPE,QSCITGRSS                                               
         BNE   ADDOC03                                                          
         LR    R0,RE                                                            
         BAS   RE,GETSEQN                                                       
         LR    RE,R0                                                            
         MVC   OCANSEQN,BOBYTE1                                                 
ADDOC03  ZAP   OCANCASH,0(8,RF)                                                 
         LA    R1,4(R1)            BUMP TO NEXT                                 
         LA    RE,1(RE)            INCREMENT COUNT                              
         LA    R2,OCANTRYL(R2)                                                  
         B     ADDOC02                                                          
*                                                                               
ADDOC04  LA    RF,BOCASCI                                                       
ADDOC04A CLI   0(RF),0                                                          
         BE    ADDOC06                                                          
         MVC   0(OCANTRYL,R2),0(RF)                                             
         LA    R2,OCANTRYL(R2)                                                  
         LA    RE,1(RE)                                                         
         LA    RF,OCANTRYL(RF)                                                  
         B     ADDOC04A                                                         
ADDOC06  SR    R2,R3                                                            
         STC   R2,OCALN                                                         
         STC   RE,OCAINDS                                                       
         GOTO1 AADDXTRA                                                         
ADDOCAX  B     EXITY                                                            
         DROP  R2                                                               
*                                                                               
GETSEQN  NTR1                                                                   
         LA    RF,1                START WITH SEQUENCE 1                        
         SR    R1,R1                                                            
         L     RE,POSTXTRA         LIST OF EXTRA ELEMENTS                       
GET02    CLI   0(RE),0             TEST END OF ELEMENTS                         
         BE    GETS06                                                           
         CLI   0(RE),SCIELQ                                                     
         BNE   GETS04                                                           
         CLI   SCITYPE-SCIELD(RE),SCITGRSS                                      
         BE    GETS06                                                           
         LA    RF,1(RF)            INCREMENT SEQUENCE                           
GETS04   IC    R1,1(RE)                                                         
         AR    RE,R1               NEXT ELEMENT                                 
         B     GET02                                                            
GETS06   STC   RF,BOBYTE1                                                       
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*              BUILD SPECIAL SCIEL                                   *          
**********************************************************************          
         SPACE 1                                                                
BVATOCA  DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         LA    R0,MAXVATQ          MAXIMUM VAT TYPES                            
         LA    RE,BOCASCI          OUTPUT BLOCK                                 
         LA    R1,2                START SEQUENCE (AFTER COMM)                  
BVAT02   CLI   0(RE),0             FIND EMPTY SLOT                              
         BE    BVAT04                                                           
         LA    R1,1(R1)            INCREMENT ELEMENT SEQUENCE                   
         LA    RE,(2*OCANTRYL)(RE)                                              
         BCT   R0,BVAT02                                                        
         USING OCANTRY,RE                                                       
BVAT04   MVC   OCANTYPE,TLKUVATT                                                
         NI    OCANTYPE,X'FF'-X'40'                                             
         STC   R1,OCANSEQN         ELEMENT SEQUENCE                             
         ZAP   OCANCASH,TLDUVTFA   VATABLE                                      
         MVC   OCANTRYL(OCANTR1L,RE),OCANTRY                                    
         LA    RE,OCANTRYL(RE)                                                  
         OI    OCANTYPE,X'40'                                                   
         ZAP   OCANCASH,TLDUVTF    VAT                                          
         B     EXITY                                                            
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
*              BUILD US STYLE GST/PST SCIELS FOR INCOME/DEBTORS       *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
GSTPST   DS    0H                                                               
         USING *,R8                                                             
*&&US                                                                           
         TM    BCGLOB1,BCGLVAT     US STYLE GST/PST SCIELS                      
         BZ    GSPSX                                                            
         MVC   BTLKEY,TLKEY                                                     
         XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,TLKUVATQ                                                  
         LA    R1,TSARDH                                                        
         B     *+8                                                              
GSPS10   LA    R1,TSANXT                                                        
         GOTO1 ATSARIO,(R1)                                                     
         BL    GSPS20                                                           
         CLI   TLKSES,TLKUVATQ                                                  
         BNE   GSPS20                                                           
         LA    RF,BOELEM                                                        
         USING SCIELD,RF                                                        
         XC    SCIEL(SCILN3Q),SCIEL                                             
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITTAXP                                                 
         MVC   SCISUBPT,TLKUVATT                                                
         ZAP   SCIAMNT,TLDUVAT                                                  
         ZAP   SCIBASE,TLDUVATA                                                 
         DROP  RF                                                               
         GOTO1 AADDXTRA                                                         
         B     GSPS10                                                           
*                                                                               
GSPS20   TM    BCGLOB1,BCGLPST                                                  
         BZ    GSPS40                                                           
         XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,TLKUPSTQ                                                  
         LA    R1,TSARDH                                                        
         B     *+8                                                              
GSPS30   LA    R1,TSANXT                                                        
         GOTO1 ATSARIO,(R1)                                                     
         BL    GSPS40                                                           
         CLI   TLKSES,TLKUPSTQ                                                  
         BNE   GSPS40                                                           
         LA    RF,BOELEM                                                        
         USING SCIELD,RF                                                        
         XC    SCIEL(SCILN3Q),SCIEL                                             
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITTQST                                                 
         MVC   SCISUBPR,TLKUPSTP                                                
         MVC   SCISUBPT,TLKUPSTT                                                
         ZAP   SCIAMNT,TLDUPST                                                  
         ZAP   SCIBASE,TLDUPSTA                                                 
         DROP  RF                                                               
         GOTO1 AADDXTRA                                                         
         B     GSPS30                                                           
*                                                                               
GSPS40   MVC   TLKEY,BTLKEY        RESTORE TSAR KEY                             
         LA    R1,TSARDH                                                        
         GOTO1 ATSARIO,(R1)                                                     
*&&                                                                             
GSPSX    B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET SCIELD FROM RECORD (ELEMENT IS ADDED IF NOT THERE)   *         
*                                                                     *         
* NTRY: P1 = A(RECORD)                                                *         
*       P2 = SCITYPE VALUE                                            *         
*       P3 = SCILN VALUE                                              *         
* EXIT: P4 = A(SCIELD ON RECORD)                                      *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
GETSCI   DS    0H                                                               
         USING *,R8                                                             
         LR    R6,R1                                                            
         LM    R2,R4,0(R6)         R3 = SCITYPE / R4 = SCILN                    
         USING ACCRECD,R2          R2 = A(RECORD)                               
*                                                                               
         LA    RE,ACCRFST                                                       
         USING SCIELD,RE                                                        
         XR    RF,RF                                                            
GSCI02   CLI   SCIEL,0                                                          
         BE    GSCI10                                                           
         CLI   SCIEL,SCIELQ                                                     
         BNE   *+12                                                             
         CLM   R3,1,SCITYPE                                                     
         BE    *+12                                                             
         IC    RF,SCILN                                                         
         BXH   RE,RF,GSCI02                                                     
         CLM   R4,1,SCILN                                                       
         BE    GETSCIX                                                          
         MVI   SCIEL,FF            WRONG LENGTH - DELETE ELEMENT                
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('FF',ACCRECD),0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GSCI10   DS    0H                                                               
         LA    RE,BOELEM                                                        
         MVI   SCIEL,SCIELQ                                                     
         STC   R4,SCILN                                                         
         STC   R3,SCITYPE                                                       
         ZAP   SCIAMNT,BCPZERO                                                  
         ZAP   SCIADMN,BCPZERO                                                  
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),ACCRECD,BOELEM,0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,16(R1)                                                        
*                                                                               
GETSCIX  ST    RE,12(R6)           SAVE A(SCIELD) FOR CALLER                    
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         ORG   LTORG                                                            
         LTORG                                                                  
         SPACE 1                                                                
ACCMST   DC    C'ACCMST '                                                       
ULBIL    DC    C'11'                                                            
ULREV    DC    C'12'                                                            
UL1R     DC    C'1R'                                                            
UL1P     DC    C'1P'                                                            
         SPACE 1                                                                
         DS    (LTORGX-*)X                                                      
         ORG                                                                    
         SPACE 1                                                                
***********************************************************************         
* GENERAL EQUATES                                                     *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* LOCAL W/S                                                           *         
***********************************************************************         
         SPACE 1                                                                
BWORKD   DSECT                                                                  
*                                                                               
AREROUT  DS    0A                                                               
APRCTRNS DS    A                                                                
ATRNGET  DS    A                                                                
ATRNCST  DS    A                                                                
ATRNVAT  DS    A                                                                
ATRNWUP  DS    A                                                                
ATRNPST  DS    A                                                                
ATRNIST  DS    A                                                                
ATRNREP  DS    A                                                                
ATRNUPD  DS    A                                                                
AUPDWCT  DS    A                                                                
ABLDPAS  DS    A                                                                
ASETVAT  DS    A                                                                
AINIPST  DS    A                                                                
APSTIST  DS    A                                                                
APSTBIL  DS    A                                                                
APSTINC  DS    A                                                                
APSTDEB  DS    A                                                                
APSTDSC  DS    A                                                                
APSTVAT  DS    A                                                                
APSTPST  DS    A                                                                
APSTCST  DS    A                                                                
APSTWUP  DS    A                                                                
APSTREP  DS    A                                                                
AUPDPRB  DS    A                                                                
AHDRMNF  DS    A                                                                
AHDRPCB  DS    A                                                                
APSTACCR DS    A                                                                
APSTTRNA DS    A                                                                
APSTDIFF DS    A                                                                
AADDOCA  DS    A                                                                
ABVATOCA DS    A                                                                
AGSTPST  DS    A                                                                
AGETSCI  DS    A                                                                
AREROUTN EQU   (*-AREROUT)/L'AREROUT                                            
         DS    (AROUTN-AREROUTN)X ENSURE AROUTN=AREROUTN                        
         DS    (AREROUTN-AROUTN)X                                               
*                                                                               
BAPTAEL  DS    A                   A(PENDING PTA ELEMENT)                       
BADVCNT  DS    H                   ADVANCE COUNT                                
BLSTWC   DS    CL2                 PREVIOUS WORKCODE                            
BWCSRG   DS    CL1                 SURCHARGABLE WORKCODE FLAG                   
BWCTIM   DS    CL1                 TIME WORKCODE FLAG                           
BCSTNM   DS    CL(L'NAMEREC)       CLIENT COSTING ACCOUNT NAME                  
BULBNM   DS    CL(L'NAMEREC)       BILLINGS COSTING ACCOUNT NAME                
BULRNM   DS    CL(L'NAMEREC)       REVENUE COSTING ACCCOUNT NAME                
BSUL     DS    CL2                                                              
BSANAL   DS    CL(L'ACTKACT)       SAVE COSTING CODE                            
BSICST   DS    CL(L'ACTKULA)       SI ACCOUNT ANALYSIS CODE                     
BVATAC   DS    CL(L'ACTKULA)       VAT ACCOUNT FOR GERMAN ATTRIBUTE             
BINDS    DS    XL1                 FLAG BYTE                                    
BIREVBL  EQU   X'80'               BILL INCLUDES REVERSALS                      
BIABAMT  EQU   X'40'               ACCRUALS ARE ABSOLUTE AMOUNTS                
BIADV    EQU   X'20'               PC BILLING ADVANCES IN BILL                  
BBLMCS   DS    CL2                 SAVED CHARACTER FORMAT MOA                   
BPSTAS   DS    PL8                 SAVED TRANSACTION AMOUNT                     
BPSTAC   DS    CL14                SAVED POSTING ACCOUNT                        
BMNTH1   DS    PL8                 SAVED MONTH 1 AMOUNT                         
BTARGT   DS    PL8                 TARGET AMOUNT WHEN ACCRUING                  
BSECCOST DS    PL8                 SAVED SECOND CURRENCY COST AMT               
BSANAC   DS    CL15                SALES ACCOUNT FROM PRODUCT                   
BSANNM   DS    CL36                NAME FOR SALES ACCOUNT                       
BTLKEY   DS    XL(L'TLKEY)         SAVED TLKEY                                  
BMEDNM   DS    XL(L'PMDDESC)       MEDIA NAME FOR BILLING SOURCE                
BMEDIN   DS    XL(L'PMDCOM1)       MEDIA RECORD INCOME A/C                      
MAXVATQ  EQU   4                                                                
OLDSEQ   DS    XL2                                                              
NEWSEQ   DS    XL2                                                              
BOCASCI  DS    (MAXVATQ)XL(OCANTRYL+1)                                          
*                                                                               
BVRATT   DS    (TLDUVTMX)XL(L'VTCTYPE+L'VTCRATE)                                
*                                                                               
BXTRAS   DS    XL(L'PBXTRA)                                                     
*                                                                               
BIOAREA5 DS    (IOAREALN)X                                                      
*                                                                               
         DS    (OVERWRKL-(*-BWORKD))X                                           
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBLINK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBLINK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031ACCLB1F   03/13/01'                                      
         END                                                                    
