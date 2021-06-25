*          DATA SET ACREPM206  AT LEVEL 023 AS OF 01/28/13                      
*PHASE ACM206A                                                                  
*INCLUDE CENTER                                                                 
*INCLUDE CONVMOS                                                                
*INCLUDE DATVAL                                                                 
*INCLUDE DLFLD                                                                  
*INCLUDE PERCALL                                                                
*INCLUDE PERVERT                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE QSORT                                                                  
*INCLUDE UNDERLIN                                                               
*INCLUDE ACSLRY                                                                 
         TITLE 'COMMON CSECT/TABLES/EXTERN'                                     
ACM206   CSECT                                                                  
         PRINT NOGEN                                                            
RC       EQU   12                                                               
         USING ACWORKD,RC                                                       
RA       EQU   10                                                               
         USING MANC,RA                                                          
MANC     DS    0F                                                               
         DC    AL4(ENTRYTAB-ACM206)    DISPLACEMENT TO ENTRIES                  
         ORG   *-4                                                              
       ++INCLUDE ACAPGMANC                                                      
         EJECT                                                                  
***********************************************************************         
* DISPLACEMENT LIST FOR CONTROL TABLES                                *         
***********************************************************************         
         SPACE 1                                                                
ENTRYTAB DC    AL2(TABLOW-ACM206),AL2(ATABLOW-MANC)                             
         DC    AL2(TABHI-ACM206),AL2(ATABHI-MANC)                               
         DC    AL2(BUCKLST-ACM206),AL2(ABUCKLST-MANC)                           
         DC    AL2(FROMLST-ACM206),AL2(AFROMLST-MANC)                           
         DC    AL2(TYPELST-ACM206),AL2(ATYPELST-MANC)                           
         DC    AL2(TYPEBL0-ACM206),AL2(ATYPEBL0-MANC)                           
         DC    AL2(TYPCTT-ACM206),AL2(ATYPCTT-MANC)                             
         DC    AL2(TYPEDIT-ACM206),AL2(ATYPEDIT-MANC)                           
         DC    AL2(COLNTAB-ACM206),AL2(ACOLNTAB-MANC)                           
         DC    AL2(LNSTL-ACM206),AL2(ALNSTL-MANC)                               
         DC    AL2(MEDN-ACM206),AL2(AMEDN-MANC)                                 
         DC    AL2(AGYADTE-ACM206),AL2(AAGYDTE-MANC)                            
         DC    AL2(OFFADTE-ACM206),AL2(AOFFDTE-MANC)                            
         DC    AL2(HRSTAB-ACM206),AL2(AHRSTAB-MANC)                             
         DC    AL2(SPBTAB-ACM206),AL2(ASPBTAB-MANC)                             
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* EQUATES FOR BUFFERS                                                 *         
***********************************************************************         
         SPACE 1                                                                
*              ABOVE THE LINE                                                   
*                                                                               
CONBK    EQU   (((CONLN+15)/8)*8)                                               
SRTBK    EQU   (((SRTLN+15)/8)*8)                                               
SLTBK    EQU   ((((SLTLN*SLTMX)+15)/8)*8)                                       
LKTBK    EQU   ((((LKTLN*LKTMX)+15)/8)*8)                                       
*                                                                               
*              BELOW THE LINE                                                   
*                                                                               
SLPBK    EQU   ((((SLPLNQ*1)+15)/8)*8)                                          
CNSBK    EQU   ((((CONLN*1)+15)/8)*8)                                           
CXTBK    EQU   (((CXTLN+15)/8)*8)                                               
NMTBK    EQU   ((((NMTLN*NMTMX)+15)/8)*8)                                       
ACTBK    EQU   ((((ACTLN*ACTMX)+15)/8)*8)                                       
RPTBK    EQU   ((((RPTLN*RPTMX)+15)/8)*8)                                       
IOTBK    EQU   ((((RECLN)+15)/8)*8)                                             
USTBK    EQU   ((((USRLN)+15)/8)*8)                                             
OPTBK    EQU   ((((OPTLN*OPTMX)+15)/8)*8)                                       
LVTBK    EQU   ((((LVTLN*LVTMX)+15)/8)*8)                                       
RLTBK    EQU   ((((RLTLN*RLTMX)+15)/8)*8)                                       
CPTBK    EQU   ((((CPTLN*CPTMX)+15)/8)*8)                                       
FPTBK    EQU   ((((FPTLN)+15)/8)*8)                                             
MWTBK    EQU   ((((MWTLN*MWTMX)+15)/8)*8)                                       
KYTBK    EQU   ((((KYTLN*KYTMX)+15)/8)*8)                                       
CATBK    EQU   ((((CATLN*CATMX)+15)/8)*8)                                       
WCTBK    EQU   ((((WCTLN*WCTMX)+15)/8)*8)                                       
BDTBK    EQU   ((((BDTLN*BDTMX)+15)/8)*8)                                       
HKTBK    EQU   ((((HKTLN)+15)/8)*8)                                             
NRTBK    EQU   ((((NRTLN*NRTMX)+15)/8)*8)                                       
PLTBK    EQU   ((((PLTLN*PLTMX)+15)/8)*8)                                       
PETBK    EQU   ((((PETLN*PETMX)+15)/8)*8)                                       
PERBK    EQU   ((((PERLNQ)+15)/8)*8)                                            
DLTBK    EQU   300                                                              
         TITLE 'TABLES DYNAMIC STORAGE REQUIRMENTS'                             
***********************************************************************         
* TABLE DEFINITIONS                                                   *         
* 8 BYTE NAME                                                         *         
* 4 BYTE LENGTH                                                       *         
* 2 BYTE DISPLACEMENT TO A(XX) INTO MAND                              *         
* 1 BYTE FILL CHARACTER                                               *         
* 1 BYTE SPARE                                                        *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
TABLOW   DS    0CL16                                                            
USERC    DC    CL8'*USER***',AL4(USTBK),AL2(AUSER-MANC),X'00',X'00'             
HOOKCODE DC    CL8'*HOOK***',AL4(HKTBK),AL2(AHOOKCDE-MANC),X'00',X'00'          
CONADS   DC    CL8'*CONADS*',AL4(CATBK),AL2(ACONADS-MANC),X'00',X'00'           
CONEXE   DC    CL8'*CONEXE*',AL4(CXTBK),AL2(ACONEXE-MANC),X'00',X'00'           
NAMES    DC    CL8'*NAMES**',AL4(NMTBK),AL2(ANAMES-MANC),X'00',X'00'            
ACCUM    DC    CL8'*ACCUM**',AL4(ACTBK),AL2(AACCUM-MANC),X'00',X'00'            
REPSTACK DC    CL8'*RSTACK*',AL4(RPTBK),AL2(ARSTACK-MANC),X'00',X'00'           
IO       DC    CL8'*APGIO**',AL4(IOTBK),AL2(APGIO-MANC),X'00',X'00'             
SORT     DC    CL8'*APGSORT',AL4(IOTBK),AL2(APGSORT-MANC),X'00',X'00'           
NAMEOPT  DC    CL8'*OPTNM**',AL4(OPTBK),AL2(ANAMEOPT-MANC),X'00',X'00'          
LEVPOOL  DC    CL8'*LEVELS*',AL4(LVTBK),AL2(ALEVPOOL-MANC),X'00',X'00'          
RLPOOL   DC    CL8'*RLPOOL*',AL4(RLTBK),AL2(ARLPOOL-MANC),X'40',X'00'           
CPOOL    DC    CL8'*CPOOL**',AL4(CPTBK),AL2(ACPOOL-MANC),X'FF',X'00'            
FORMPOOL DC    CL8'*FPOOL**',AL4(FPTBK),AL2(AFRMPOOL-MANC),X'FF',X'00'          
MANWK    DC    CL8'*MANWK**',AL4(MWTBK),AL2(AMANWK-MANC),X'00',X'00'            
WCTAB    DC    CL8'*WCTAB**',AL4(WCTBK),AL2(AWCTAB-MANC),X'00',X'00'            
BUDGTITS DC    CL8'*BUDTIT*',AL4(BDTBK),AL2(ABUDGTIT-MANC),X'00',X'00'          
NARBK    DC    CL8'*NARR***',AL4(NRTBK),AL2(ANARBK-MANC),X'00',X'00'            
XPLN     DC    CL8'*PLINES*',AL4(PLTBK),AL2(AXPLN-MANC),X'40',X'00'             
PELVT    DC    CL8'*PETIME*',AL4(PETBK),AL2(APELVT-MANC),X'00',X'00'            
SPLWRK   DC    CL8'*SLPWRK*',AL4(SLPBK),AL2(ASLPWRK-MANC),X'00',X'00'           
REPCOPY  DC    CL8'*REPCOPY',AL4(RPTMX),AL2(AREPCOPY-MANC),X'00',X'00'          
DWNBUF   DC    CL8'*DWNBUF*',AL4(DLTBK),AL2(ADWNBUF-MANC),X'00',X'00'           
CONSBF   DC    CL8'*CONREC*',AL4(CNSBK),AL2(ACONSR-MANC),X'00',X'00'            
PERBLK   DC    CL8'*PERBLK*',AL4(PERBK),AL2(APERBLK-MANC),X'00',X'00'           
*MNNEW                                                                          
SLPOST   DC    CL8'*SLP****',AL4(SLTBK),AL2(ASLP-MANC),X'00',X'00'              
LNKLST   DC    CL8'*LNK****',AL4(LKTBK),AL2(ALNK-MANC),X'00',X'00'              
SRTBUF   DC    CL8'*SRTBUF*',AL4(SRTBK),AL2(ASRTBF-MANC),X'00',X'00'            
CONBUF   DC    CL8'*CONBUF*',AL4(CONBK),AL2(ACONBF-MANC),X'00',X'00'            
*MNNEW                                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
*              TABLE FOR ABOVE THE LINE STORAGE                                 
*                                                                               
TABHI    DS    0CL16                                                            
*SLPOST   DC    CL8'*SLP****',AL4(SLTBK),AL2(ASLP-MANC),X'00',X'00'             
*LNKLST   DC    CL8'*LNK****',AL4(LKTBK),AL2(ALNK-MANC),X'00',X'00'             
*SRTBUF   DC    CL8'*SRTBUF*',AL4(SRTBK),AL2(ASRTBF-MANC),X'00',X'00'           
*CONBUF   DC    CL8'*CONBUF*',AL4(CONBK),AL2(ACONBF-MANC),X'00',X'00'           
         DC    X'FF'                                                            
         TITLE 'TABLES - BUCKET TYPE'                                           
**********************************************************************          
* BUCKET TYPES - SEE BTD                                             *          
**********************************************************************          
         SPACE 1                                                                
BUCKLST  DS    0C                                                               
         SPACE 1                                                                
BK$      DC    C' ',AL1(0),AL1(BK$X-BK$)             NO BUCKET                  
         DC    AL1((BK$X-*)/BTRLQ)                                              
         DC    AL1(LNDRS,BTADDD,0,0)                + DEBITS                    
         DC    AL1(LNCRS,BTADDC,0,0)                + CREDITS                   
BK$X     EQU   *                                                                
         SPACE 1                                                                
*&&US                                                                           
BKH      DC    C'H',AL1(0),AL1(BKHX-BKH)             HOURS                      
         DC    AL1((BKHX-*)/BTRLQ)                                              
         DC    AL1(LNHRS,BTADDC,0,0)                  + CRS                     
         DC    AL1(LNHRS,BTADDD,BTFPC,0)              + DRS/PROJ CON            
         DC    AL1(LNHRS,BTSUBD,BTFAH,0)              - DRS/ADJ HRS             
*                                                     TOTAL TIME                
         DC    AL1(LNTLT,BTADDC,0,0)                 + CRS                      
*                                                     CLIENT TIME               
         DC    AL1(LNCLT,BTADDC,0,BTFCLT)             + CRS                     
*                                                     TOTAL LESS LEAVE          
*                                                       ALL EXCEPT LVE          
         DC    AL1(LNTLV,BTADDC,0,BTFCLT+BTFNON+BTFPER)                         
*                                                     TOTAL EXCEPT              
*                                                     LEAVE AND PERSON          
         DC    AL1(LNTLP,BTADDC,0,BTFCLT+BTFNON)                                
BKHX     EQU   *                                                                
*&&                                                                             
*&&UK                                                                           
BKH      DC    C'H',AL1(0),AL1(BKHX-BKH)             HOURS                      
         DC    AL1((BKHX-*)/BTRLQ)                                              
         DC    AL1(LNHRS,BTADDD,0,0)                  + DRS                     
*                                                     TOTAL TIME                
         DC    AL1(LNTLT,BTADDD,0,0)                  + DRS                     
*                                                     CLIENT TIME               
         DC    AL1(LNCLT,BTADDD,0,BTFCLT)             +DRS                      
*                                                     TOTAL LESS LEAVE          
         DC    AL1(LNTLV,BTADDD,0,BTFCLT+BTFNON+BTFPER) ALL EXCEPT LVE          
*                                                     TOTAL EXCEPT              
*                                                     LEAVE AND PERSON          
         DC    AL1(LNTLP,BTADDD,0,BTFCLT+BTFNON)                                
BKHX     EQU   *                                                                
         SPACE 1                                                                
BKT      DC    C'T',AL1(0),AL1(BKTX-BKT)             HOURS ALSO T               
         DC    AL1((BKTX-*)/BTRLQ)                                              
         DC    AL1(LNHRS,BTADDD,0,0)                  + DRS                     
*                                                     TOTAL TIME                
         DC    AL1(LNTLT,BTADDD,0,0)                  + DRS                     
*                                                     CLIENT TIME               
         DC    AL1(LNCLT,BTADDD,0,BTFCLT)             +DRS                      
*                                                     TOTAL LESS LEAVE          
         DC    AL1(LNTLV,BTADDD,0,BTFCLT+BTFNON+BTFPER) ALL EXCEPT LVE          
*                                                     TOTAL EXCEPT              
*                                                     LEAVE AND PERSON          
         DC    AL1(LNTLP,BTADDD,0,BTFCLT+BTFNON)                                
BKTX     EQU   *                                                                
*&&                                                                             
         SPACE 1                                                                
BKF      DC    C'F',AL1(0),AL1(BKFX-BKF)              FEES                      
         DC    AL1((BKFX-*)/BTRLQ)                                              
         DC    AL1(LNFEE,BTADDD,0,0)                  +DRS FEES                 
         DC    AL1(LNFAJ,BTADDC,0,0)                  +CRS ADJUSTS.             
BKFX     EQU   *                                                                
         SPACE 1                                                                
BKB      DC    C'B',AL1(0),AL1(BKBX-BKB)              BENEFITS                  
         DC    AL1((BKBX-*)/BTRLQ)                                              
         DC    AL1(LNBEN,BTADDD,0,0)                  +DRS BENEFITS             
         DC    AL1(LNADM,BTADDC,0,0)                  +CRS ADMIN.               
BKBX     EQU   *                                                                
         SPACE 1                                                                
BKC      DC    C'C',AL1(0),AL1(BKCX-BKC)              COMMISSIONS               
         DC    AL1((BKCX-*)/BTRLQ)                                              
         DC    AL1(LNCMM,BTADDD,0,0)                  +DRS                      
         DC    AL1(LNCMM,BTSUBC,0,0)                  -CRS                      
BKCX     EQU   *                                                                
         SPACE 1                                                                
BKD      DC    C'D',AL1(0),AL1(BKDX-BKD)              DISCOUNTS                 
         DC    AL1((BKDX-*)/BTRLQ)                                              
         DC    AL1(LNCSD,BTADDD,0,0)                  +DRS                      
         DC    AL1(LNCSD,BTSUBC,0,0)                  -CRS                      
BKDX     EQU   *                                                                
         SPACE 1                                                                
BKG      DC    C'G',AL1(0),AL1(BKGX-BKG)              GROSS BILLINGS            
         DC    AL1((BKGX-*)/BTRLQ)                                              
         DC    AL1(LNGRB,BTADDD,0,0)                  +DRS                      
         DC    AL1(LNGRB,BTSUBC,0,0)                  -CRS                      
BKGX     EQU   *                                                                
         SPACE 1                                                                
BKN      DC    C'N',AL1(0),AL1(BKNX-BKN)              NET                       
         DC    AL1((BKNX-*)/BTRLQ)                                              
         DC    AL1(LNNET,BTADDD,0,0)                  +DRS                      
         DC    AL1(LNNET,BTSUBC,0,0)                  -CRS                      
BKNX     EQU   *                                                                
         SPACE 1                                                                
*&&US                                                                           
BKA      DC    C'A',AL1(0),AL1(BKAX-BKA)              ADJUSTMENTS               
         DC    AL1((BKAX-*)/BTRLQ)                                              
         DC    AL1(LNAJD,BTADDD,0,0)                  +DRS                      
         DC    AL1(LNAJR,BTADDC,0,0)                  +CRS                      
BKAX     EQU   *                                                                
*&&                                                                             
         SPACE 1                                                                
BKX      DC    C'X',AL1(0),AL1(BKXX-BKX)               VAT                      
         DC    AL1((BKXX-*)/BTRLQ)                                              
         DC    AL1(LNVAT,BTADDD,0,0)                   +DRS                     
         DC    AL1(LNVAT,BTADDC,0,0)                   +CRS                     
         DC    AL1(LNBVT,BTADDC,0,0)                   +CRS BVAT                
         DC    AL1(LNUVT,BTADDD,0,0)                   +DRS UVAT                
         DC    AL1(LNUVT,BTSUBC,0,0)                   -CRS UVAT                
BKXX     EQU   *                                                                
         SPACE 1                                                                
BK1N     DC    C'1',AL1(BTTNC),AL1(BK1NX-BK1N)        SALARY                    
         DC    AL1((BK1NX-*)/BTRLQ)                                             
         DC    AL1(LNASL,BTADDD,0,0)                   +DRS/PERSON              
         DC    AL1(LNASL,BTADDC,0,0)                   +CRS/CLIENT              
         DC    AL1(LNDRS,BTADDD,0,0)                   + DEBITS                 
         DC    AL1(LNCRS,BTADDC,0,0)                   + CREDITS                
BK1NX    EQU   *                                                                
         SPACE 1                                                                
BK2N     DC    C'2',AL1(BTTNC),AL1(BK2NX-BK2N)        BENEFITS                  
         DC    AL1((BK2NX-*)/BTRLQ)                                             
         DC    AL1(LNABN,BTADDD,0,0)                   +DRS/PERSON              
         DC    AL1(LNABN,BTADDC,0,0)                   +CRS/CLIENT              
         DC    AL1(LNDRS,BTADDD,0,0)                   + DEBITS                 
         DC    AL1(LNCRS,BTADDC,0,0)                   + CREDITS                
BK2NX    EQU   *                                                                
         SPACE 1                                                                
BK3N     DC    C'3',AL1(BTTNC),AL1(BK3NX-BK3N)        PENSIONS                  
         DC    AL1((BK3NX-*)/BTRLQ)                                             
         DC    AL1(LNAPN,BTADDD,0,0)                   +DRS/PERSON              
         DC    AL1(LNAPN,BTADDC,0,0)                   +CRS/CLIENT              
         DC    AL1(LNDRS,BTADDD,0,0)                   + DEBITS                 
         DC    AL1(LNCRS,BTADDC,0,0)                   + CREDITS                
BK3NX    EQU   *                                                                
         SPACE 1                                                                
BKCN     DC    C'C',AL1(BTTNC),AL1(BKCNX-BKCN)        CORPORATE OH              
         DC    AL1((BKCNX-*)/BTRLQ)                                             
         DC    AL1(LNDRS,BTADDD,BTFPE,0)              + DEBITS PERSON           
         DC    AL1(LNCRS,BTADDC,BTFCL,0)              + CREDITS CLIENT          
BKCNX    EQU   *                                                                
         SPACE 1                                                                
BKDN     DC    C'D',AL1(BTTNC),AL1(BKDNX-BKDN)        DEPT OH                   
         DC    AL1((BKDNX-*)/BTRLQ)                                             
         DC    AL1(LNDRS,BTADDD,BTFPE,0)              + DEBITS                  
         DC    AL1(LNCRS,BTADDC,BTFCL,0)              + CREDITS CLIENT          
BKDNX    EQU   *                                                                
         SPACE 1                                                                
BKON     DC    C'O',AL1(BTTNC),AL1(BKONX-BKON)        OFFICE OH                 
         DC    AL1((BKONX-*)/BTRLQ)                                             
         DC    AL1(LNDRS,BTADDD,BTFPE,0)              + DEBITS                  
         DC    AL1(LNCRS,BTADDC,BTFCL,0)              + CREDITS CLIENT          
BKONX    EQU   *                                                                
         SPACE 1                                                                
BKGN     DC    C'G',AL1(BTTNC),AL1(BKGNX-BKGN)        GROUP OH                  
         DC    AL1((BKGNX-*)/BTRLQ)                                             
         DC    AL1(LNDRS,BTADDD,BTFPE,0)              + DEBITS                  
         DC    AL1(LNCRS,BTADDC,BTFCL,0)              + CREDITS CLIENT          
BKGNX    EQU   *                                                                
         SPACE 1                                                                
BKEND    DC    X'FF'               END OF TABLE                                 
         EJECT 'TABLES - "FROM" LIST'                                           
***********************************************************************         
* "FROM" DATA TABLE                                                   *         
***********************************************************************         
         DS    0F                                                               
FROMLST  DC    S(CURRACC+1,CURRACC)     AU=1                                    
         DC    S(CURRACC+3,CURRACC)     AC=2                                    
         DC    S(CURRCON+1,CURRCON)     CU=3                                    
         DC    S(CURRCON+3,CURRCON)     CA=4                                    
         DC    S(CURRFLT1,0)            F1=5                                    
         DC    S(CURRFLT2,0)            F2=6                                    
         DC    S(CURRFLT3,0)            F3=7                                    
         DC    S(CURRWC,0)              WC=8                                    
         DC    S(CURRDATE,0)            DT=9                                    
         DC    S(CURRREF,0)             RF=10                                   
         DC    S(CURRNUM,0)             NU=11                                   
         DC    S(CURRANAL,0)            AN=12                                   
         DC    S(CURRCON+1,0)           T1=13                                   
         DC    S(CURRTE2,0)             T2=14                                   
         DC    A(0)                     L1=15                                   
         DC    A(0)                     L2=16                                   
         DC    A(0)                     L3=17                                   
         DC    A(0)                     L4=18                                   
         DC    S(NUMLEVS,0)             LV=19                                   
         DC    A(0)                     ME=20                                   
         DC    S(PROGPROF,0)            01=21                                   
         DC    S(PROGPROF+1,0)          02=22                                   
         DC    S(PROGPROF+2,0)          03=23                                   
         DC    S(PROGPROF+3,0)          04=24                                   
         DC    S(PROGPROF+4,0)          05=25                                   
         DC    S(PROGPROF+5,0)          06=26                                   
         DC    S(PROGPROF+6,0)          07=27                                   
         DC    S(PROGPROF+7,0)          08=28                                   
         DC    S(PROGPROF+8,0)          09=29                                   
         DC    S(PROGPROF+9,0)          10=30                                   
         DC    S(PROGPROF+10,0)         11=31                                   
         DC    S(PROGPROF+11,0)         12=32                                   
         DC    S(PROGPROF+12,0)         13=33                                   
         DC    S(PROGPROF+13,0)         14=34                                   
         DC    S(PROGPROF+14,0)         15=35                                   
         DC    S(PROGPROF+15,0)         16=36                                   
         DC    S(CURROFC,0)             TA=37                                   
         DC    S(CURRBTCH,0)            TB=38                                   
         DC    S(CURRNARR,0)            TN=39                                   
         DC    S(QRECORD,0)             RQ=40                                   
         DC    S(QOPT1,0)               Q1=41                                   
         DC    S(QOPT2,0)               Q2=42                                   
         DC    S(QOPT3,0)               Q3=43                                   
         DC    S(QOPT4,0)               Q4=44                                   
         DC    S(QOPT5,0)               Q5=45                                   
         DC    S(QOPT6,0)               Q6=46                                   
         DC    S(QOPT7,0)               Q7=47                                   
         DC    S(CURROTH,0)             OT=48                                   
         DC    S(CURRFLT4,0)            F4=49                                   
         DC    S(CURRFLT5,0)            F5=50                                   
         DC    S(CURRCAC1+3,CURRCAC1)   C1=51                                   
         DC    S(CURRCAC2+3,CURRCAC2)   C2=52                                   
         DC    S(CURRCAC3+3,CURRCAC3)   C3=53                                   
         DC    S(CURRCAC4+3,CURRCAC4)   C4=54                                   
         DC    A(2)                     RL=55                                   
         DC    S(CURRAD,0)              AD=56                                   
         DC    S(CURRMD,0)              MD=57                                   
         DC    S(CURREP,0)              EP=58                                   
         DC    S(CURRDU,0)              DU=59                                   
         DC    S(CURRACC+3,0)           AL00=60                                 
         DC    S(CURRCON+3,0)           CL=61                                   
         DC    S(CURRTYPE,0)            TT=62                                   
         DC    S(ACTACC+1,ACTACC)       RU=63                                   
         DC    S(ACTACC+3,ACTACC)       RA=64                                   
         DC    S(CURRBBFD,0)            BF=65                                   
         DC    S(CURRHIRE,0)            HI=66                                   
         DC    S(CURRTERM,0)            TR=67                                   
         DC    S(CURRDUE,0)             DD=68                                   
         DC    A(0)                       =69                                   
         DC    A(0)                     ST=70                                   
         DC    A(0)                     EN=71                                   
         DC    A(0)                     TY=72                                   
         DC    S(CURRBKT,0)             BK=73                                   
         DC    S(CURRJOB,CURRJOB)       JB=74                                   
         DC    S(CURRTSK,0)             TK=75                                   
         DC    S(CURRCG,0)              CG=76                                   
         DC    S(CURRNT,0)              NT=77 TYPE OF 1N TIME                   
         DC    S(QRECORD2,0)            R2=78 SECOND REQUEST CARD               
         DC    S(CURRTIME,0)            TI=79 B, N OR R (TIME)                  
         TITLE 'TABLES - TYPE LIST'                                             
***********************************************************************         
* TYPE LIST TABLE - SEE TYPLD                                         *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*TYPELST'                                                      
TYPELST  DC    (L'TYPLEN)X'00'                     DUMMY FOR ALIGNMENT          
         DC    AL1(KWSAL,LNDRS,ED0)                SALARY                       
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWHRS,LNHRS,ED2)                HOURS                        
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWGRS,LNHRS,EP2)                CLIENT 'HOURS' GRS           
         DC    AL1(0,0),C'+',AL2(TYGRS-ACM206)                                  
         DC    AL1(KWNET,LNHRS,EP2)                CLIENT 'HOURS' NET           
         DC    AL1(0,0),C'+',AL2(TYNET-ACM206)                                  
         DC    AL1(KWPCT,LNHRS,EP1)                CLIENT 'HOURS' PCT           
         DC    AL1(0,0),C'+',AL2(TYPCT-ACM206)                                  
         DC    AL1(KW006,LNS31,ED0)                BUDGET SALARY                
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KW007,LNS32,ED2)                       HOURS                 
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KW008,LNS33,EP1)                       PCT.                  
         DC    AL1(0,0),C'+',AL2(0)                                             
*&&US                                                                           
         DC    AL1(KWTMP,LNTMP,ED0)                TEMP.                        
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWOVR,LNOVT,ED0)                OVERTIME                     
         DC    AL1(0,0),C'+',AL2(0)                                             
*&&                                                                             
*&&UK                                                                           
         DC    AL1(KWUVT,LNUVT,ED0)                UVAT(UK)                     
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBVT,LNBVT,ED0)                BVAT(UK)                     
         DC    AL1(0,0),C'+',AL2(0)                                             
*&&                                                                             
         DC    AL1(KWBON,LNBON,ED0)                BONUS                        
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBEN,LNBEN,ED0)                BENEFITS                     
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWFEE,LNFEE,ED0)                FEES                         
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWDRS,LNDRS,ED2)                DEBITS                       
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWCRS,LNCRS,ED2)                CREDITS                      
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBAL,LNDRS,ED2)                BALANCE - DEBITS             
         DC    AL1(0,0),C'+',AL2(TYBAL-ACM206)                                  
         DC    AL1(KWCSD,LNCSD,ED2)                CASH DISCOUNT                
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWCOM,LNCMM,ED2)                COMMISSIONS                  
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBIL,LNGRB,ED0)                GROSS BILLING                
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBAS,LNSAL,ED0)                BASIC SALARY                 
         DC    AL1(TYPMWR,0),C'+',AL2(0)                                        
         DC    AL1(KWTSL,LNSAL,ED0)                TOTAL SALARY  BASIC          
         DC    AL1(0,0),C'+',AL2(TYTSL-ACM206)                                  
         DC    AL1(KWNBL,LNGRB,ED0)                NET BILLING - GROSS          
         DC    AL1(0,0),C'+',AL2(TYNBL-ACM206)                                  
         DC    AL1(KWDR0,LNDRS,ED0)                DR0                          
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWCR0,LNCRS,ED0)                CR0                          
         DC    AL1(0,0),C'+',AL2(0)                                             
TYPEBL0  DC    AL1(KWBL0,LNDRS,ED0)                BAL0 - DEBITS                
         DC    AL1(0,0),C'+',AL2(TYBL0-ACM206)                                  
         DC    AL1(KWORD,LNORD,ED0)                ORDERED                      
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWUBL,LNORD,ED0)                BILLABLE - ORDERED           
         DC    AL1(0,0),C'+',AL2(TYBLB-ACM206)                                  
         DC    AL1(KWCLD,LNCLD,ED0)                CLEARED                      
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWUCL,LNORD,ED0)                UNCLEAR - ORDERED            
         DC    AL1(0,0),C'+',AL2(TYUCL-ACM206)                                  
         DC    AL1(KWADM,LNADM,ED0)                ADMIN                        
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBG1,LNBG1,EDB)                BUDGET - 1                   
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBG2,LNBG2,EDB)                       - 2                   
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBG3,LNBG3,EDB)                       - 3                   
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBG4,LNBG4,EDB)                       - 4                   
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBG5,LNBG5,EDB)                       - 5                   
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBG6,LNBG6,EDB)                       - 6                   
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBG7,LNBG7,EDB)                       - 7                   
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBG8,LNBG8,EDB)                       - 8                   
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBG9,LNBG9,EDB)                       - 9                   
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWNSL,LNDRS,ED0)                NSAL - DEBITS SALARY         
         DC    AL1(0,0),C'+',AL2(TYNSL-ACM206)                                  
         DC    AL1(KWBLF,LNNUL,ED0)                BALFWD                       
         DC    AL1(TYPNON,0),C'+',AL2(0)                                        
         DC    AL1(KWBF0,LNNUL,ED0)                BBF0                         
         DC    AL1(TYPNON,0),C'+',AL2(0)                                        
         DC    AL1(KWOES,LNNUL,ED0)                OEST                         
         DC    AL1(TYPNON,0),C'+',AL2(0)                                        
         DC    AL1(KWOE0,LNNUL,ED0)                OEST0                        
         DC    AL1(TYPNON,0),C'+',AL2(0)                                        
         DC    AL1(KWCES,LNNUL,ED0)                CEST                         
         DC    AL1(TYPNON,0),C'+',AL2(0)                                        
         DC    AL1(KWCE0,LNNUL,ED0)                CEST0                        
         DC    AL1(TYPNON,0),C'+',AL2(0)                                        
         DC    AL1(KWPES,LNNUL,ED0)                PEST                         
         DC    AL1(TYPNON,0),C'+',AL2(0)                                        
         DC    AL1(KWPE0,LNNUL,ED0)                PEST0                        
         DC    AL1(TYPNON,0),C'+',AL2(0)                                        
*&&US                                                                           
         DC    AL1(KWXGR,LNEXG,ED2)                EXPBGR                       
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWXG0,LNEXG,ED0)                EXPBGR0                      
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWXNE,LNEXN,ED2)                EXPBNE                       
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWXN0,LNAJD,ED0)                EXPBNE0                      
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWADJ,LNAJD,ED2)                ADJD                         
         DC    AL1(0,0),C'+',AL2(0)                                             
*&&                                                                             
*&&UK                                                                           
         DC    AL1(KWVAT,LNVAT,ED2)                VAT                          
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWVT0,LNVAT,ED0)                VAT0                         
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWCHR,LNCHR,ED2)                CHOURS                       
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWCAM,LNCAM,ED2)                CAMT                         
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWCA0,LNCAM,ED0)                CAMT0                        
         DC    AL1(0,0),C'+',AL2(0)                                             
*&&                                                                             
         DC    AL1(KWAJ0,LNAJD,ED0)                ADJD0                        
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWADC,LNAJR,ED2)                ADJC                         
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWAC0,LNAJR,ED0)                ADJC0                        
         DC    AL1(TYPMWR,0),C'+',AL2(0)                                        
         DC    AL1(KWFAD,LNFAJ,ED0)                FEEADJ                       
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWADF,LNFEE,ED0)                ADJFEES                      
         DC    AL1(0,0),C'+',AL2(TYADF-ACM206)                                  
         DC    AL1(KWMWR,LNHRS,EP1)               TYPE1%  CLIENT HOURS          
         DC    AL1(0,0),C'+',AL2(TYMWR-ACM206)                                  
         DC    AL1(KWTP1,LNHRS,EP1)               TYPE1% IS CLIENT HRS          
         DC    AL1(0,0),C'+',AL2(TYTP1-ACM206)                                  
         DC    AL1(KWTOH,LNTLT,ED2)               TOTAL EMPLOYEE HOURS          
         DC    AL1(TYPMWR,0),C'+',AL2(0)                                        
         DC    AL1(KWSTH,LNSWH,ED2)               STANDARD HOURS                
         DC    AL1(TYPMWR,0),C'+',AL2(0)                                        
         DC    AL1(KWCLH,LNCLT,ED2)               CLIENT TOTAL HOURS            
         DC    AL1(TYPMWR,0),C'+',AL2(0)                                        
         DC    AL1(KWBRA,LNBLR,ED2)               BILLING RATE                  
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBRH,LNBRH,ED0)               BILLING RATE * HOURS          
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWADH,LNADH,ED2)               ADJUSTED HOURS                
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWMSH,LNMSH,ED2)               MISSING HOURS                 
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWASL,LNASL,ED0)               ALLOCATED SALARY              
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWABN,LNABN,ED0)               ALLOCATED BENEFIT             
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWAPN,LNAPN,ED0)               ALLOCATED PENSION             
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWATS,LNASL,ED0)               ALLOCATED TOTAL               
         DC    AL1(0,0),C'+',AL2(TYATS-ACM206)                                  
         DC    AL1(KWBGA,LNBGA,EDB)               BUDGET - 10                   
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBGB,LNBGB,EDB)               BUDGET - 11                   
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBGC,LNBGC,EDB)               BUDGET - 12                   
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBGD,LNBGD,EDB)               BUDGET - 13                   
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBGE,LNBGE,EDB)               BUDGET - 14                   
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWBGF,LNBGF,EDB)               BUDGET - 15                   
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWTHR,LNTHR,ED2)               TRANSACTION HOURS             
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWMTS,LNMTS,ED0)               MISSING TIMESHEETS            
         DC    AL1(TYPNAC,0),C'+',AL2(0)                                        
         DC    AL1(KWSTR,LNSTR,ED2)               STANDARD RATE                 
         DC    AL1(TYPNAC,0),C'+',AL2(0)                                        
         DC    AL1(KWST$,LNST$,ED0)               STANDARD DOLLARS              
         DC    AL1(0,0),C'+',AL2(0)                                             
*   ACCUMULATORS FOR ALLOCATED RATES AND AMOUNTS (YTD)                          
         DC    AL1(KWPSR,LNPSR,ED2)               SALARY RATE                   
         DC    AL1(TYPNAC,0),C'+',AL2(0)                                        
         DC    AL1(KWPS$,LNPS$,ED0)               SALARY DOLLARS                
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWPBR,LNPBR,ED2)               BENEFIT RATE                  
         DC    AL1(TYPNAC,0),C'+',AL2(0)                                        
         DC    AL1(KWPB$,LNPB$,ED0)               BENEFIT DOLLARS               
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWPPR,LNPPR,ED2)               PENSION RATE                  
         DC    AL1(TYPNAC,0),C'+',AL2(0)                                        
         DC    AL1(KWPP$,LNPP$,ED0)               PENSION DOLLARS               
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWPTR,LNPTR,ED2)               TOTAL RATE                    
         DC    AL1(TYPNAC,0),C'+',AL2(0)                                        
         DC    AL1(KWPT$,LNPT$,ED0)               TOTAL DOLLARS                 
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWTHP,LNTHR,EP1)               TH% OF CLIENT HRS             
         DC    AL1(0,0),C'+',AL2(TYTP1-ACM206)                                  
*  ACCUMULATORS FROM MONTHLY COMPUTATIONS (SAL+PEN+BEN)                         
         DC    AL1(KWTO$,LNTO$,ED0)  TOTAL OF MISSING+ADJUSTED+CURRENT          
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWMS$,LNMS$,ED0)  MISS T/S  $                                
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWAD$,LNAD$,ED0)  ADJUSTED  $                                
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWCU$,LNCU$,ED0)  CURRENT  $                                 
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWCUR,LNCUR,ED2)  RATE                                       
         DC    AL1(0,0),C'+',AL2(0)                                             
*  ACCUMULATORS FROM MONTHLY COMPUTATIONS (BASED ON BUCKET TYPE)                
*              WHEN BUCKET = BLANK ACCUM IS SALARY ONLY                         
*              WHEN BUCKET = P ACCUM IS PENSION ONLY                            
*              WHEN BUCKET = B ACCUM IS BENEFIT ONLY                            
         DC    AL1(KWSPBT$,LNSPBT$,ED0)    TOTAL OF MISS+ADJUST+CURRENT         
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWSPBM$,LNSPBM$,ED0)    MISS T/S  $                          
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWSPBA$,LNSPBA$,ED0)    ADJUSTED  $                          
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWSPBC$,LNSPBC$,ED0)    CURRENT  $                           
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWSPBR,LNSPBR,ED2)      RATE                                 
         DC    AL1(0,0),C'+',AL2(0)                                             
         DC    AL1(KWTLP,LNTLP,ED2)        TOTAL EMP HRS-LEAVE-PERSNL           
         DC    AL1(TYPMWR,0),C'+',AL2(0)                                        
*MN                                                                             
         DC    AL1(KWHIST,LNHIST,ED2)       STRAIGHT SALARY HISTORY             
         DC    AL1(TYPNAC,0),C'+',AL2(0)                                        
*MN                                                                             
         DC    X'FF'                                                            
*                                                                               
TYGRS    DC    AL1(LNTLV),C'X'                     X (TOTAL-L)                  
         DC    AL1(LNTLP),C'/'                     / (TOTAL-L-P)                
         DC    AL1(LNBCW),C'/'                    / BROADCAST WEEKS             
         DC    X'FF'                                                            
*                                                                               
TYNET    DC    AL1(LNBCW),C'/'                    / BROADCAST WEEKS             
         DC    X'FF'                                                            
*                                                                               
TYPCT    DC    AL1(X1000),C'X'                    X 10000                       
TYPCTT   DC    AL1(LNTLT),C'/'                    / TOTAL TIME OR               
         DC    X'FF'                                                            
*                                                                               
TYBAL    DC    AL1(LNCRS),C'-'                    -CREDITS                      
         DC    X'FF'                                                            
*                                                                               
TYTSL    DC    AL1(LNOVT),C'+'                    +TEMP                         
         DC    AL1(LNTMP),C'+'                    +OT                           
         DC    AL1(LNBON),C'+'                    +BONUS                        
         DC    X'FF'                                                            
*                                                                               
TYNBL    DC    AL1(LNCMM),C'-'                     -COMM                        
         DC    X'FF'                                                            
*                                                                               
TYBL0    DC    AL1(LNCRS),C'-'                     - CREDITS                    
         DC    X'FF'                                                            
*                                                                               
TYBLB    DC    AL1(LNGRB),C'-'                     - BILLED                     
         DC    X'FF'                                                            
*                                                                               
TYUCL    DC    AL1(LNCLD),C'-'                     - CLEARED                    
         DC    X'FF'                                                            
*                                                                               
TYNSL    DC    AL1(LNBEN),C'-'                      - BENEFITS                  
         DC    AL1(LNADM),C'-'                      - ADMIN                     
         DC    X'FF'                                                            
*                                                                               
TYADF    DC    AL1(LNFAJ),C'+'                      - FEE ADJ                   
         DC    X'FF'                                                            
*                                                                               
TYMWR    DC    AL1(X1000),C'X'               TIMES 10000                        
         DC    AL1(LNTLP),C'/'               / BY TOT - LVE - PER               
         DC    AL1(CE099),C'A'               RESULT IN WORK+16                  
         DC    AL1(LNWKW),C'+'               MWR IS WEEKS WORKED                
         DC    AL1(LNWKP),C'/'               / BY WEEKS POSSIBLE                
         DC    AL1(CE099),C'M'               X WORK+8 BY WORK+16                
         DC    AL1(CE098),C'D'               / BY RESULT BY '100'               
         DC    X'FF'                                                            
*                                                                               
TYTP1    DC    AL1(X1000),C'X'               TIMES 10000                        
         DC    AL1(LNTLP),C'/'               / BY TOT - LVE - PER               
         DC    X'FF'                                                            
*                                                                               
TYATS    DC    AL1(LNABN),C'+'               + BONUS                            
         DC    AL1(LNAPN),C'+'               + PENSION                          
         DC    X'FF'                                                            
         TITLE 'TABLES - COLUMN EDITS'                                          
***********************************************************************         
* TABLE OF COLUMN EDITS                                               *         
***********************************************************************         
*                                                                               
TYPEDIT  DS    0CL4    CODE\SPARE\FORMAT                                        
         DC    AL1(ED0,0),C'0 '                                                 
         DC    AL1(ED2,0),C'2 '                                                 
         DC    AL1(EP1,0),C'1%'                                                 
         DC    AL1(EP2,0),C'2%'                                                 
         DC    AL1(EDB,9),C'0 '                                                 
         TITLE 'TABLES - STANDARD COLUMN NAMES'                                 
***********************************************************************         
* TABLE OF STANDARD COLUMN NAMES                                      *         
***********************************************************************         
         SPACE 1                                                                
COLNTAB  DS    0H                                                               
         DC    AL1(1,00)           PERIOD                                       
         DCDDL AC#APG01,8,C                                                     
         DC    AL1(2,00)           Y.T.D.                                       
         DCDDL AC#APG02,8,C                                                     
         DC    AL1(3,00)           ROLLING                                      
         DCDDL AC#APG03,8,C                                                     
         DC    AL1(4,00)           FISCAL                                       
         DCDDL AC#APG04,8,C                                                     
         DC    AL1(5,00)           PER.LAST                                     
         DCDDL AC#APG05,8,C                                                     
         DC    AL1(6,00)           YTD.LAST                                     
         DCDDL AC#APG06,8,C                                                     
         DC    AL1(7,00)           ROL.LAST                                     
         DCDDL AC#APG07,8,C                                                     
         DC    AL1(8,00)           FSC.LAST                                     
         DCDDL AC#APG08,8,C                                                     
*                                                                               
         DC    AL1(9,01)           SALARY                                       
         DCDDL AC#APG09,8,C                                                     
         DC    AL1(9,02)           HOURS                                        
         DCDDL AC#APG10,8,C                                                     
         DC    AL1(9,03)           GROSS%                                       
         DCDDL AC#APG11,8,C                                                     
         DC    AL1(9,04)           NET %                                        
         DCDDL AC#APG12,8,C                                                     
         DC    AL1(9,05)           PERCENT                                      
         DCDDL AC#APG13,8,C                                                     
         DC    AL1(9,06)           BUDG.SAL                                     
         DCDDL AC#APG14,8,C                                                     
         DC    AL1(9,07)           BUDG.HRS                                     
         DCDDL AC#APG15,8,C                                                     
         DC    AL1(9,08)           BUDGET%                                      
         DCDDL AC#APG16,8,C                                                     
*&&US*&& DC    AL1(9,09)           TEMPS                                        
*&&US*&& DCDDL AC#APG17,8,C                                                     
*&&US*&& DC    AL1(9,10)           OVERTIME                                     
*&&US*&& DCDDL AC#APG18,8,C                                                     
*&&UK*&& DC    AL1(9,09)           UNB VAL                                      
*&&UK*&& DCDDL AC#APG19,8,C                                                     
*&&UK*&& DC    AL1(9,10)           BL VAT                                       
*&&UK*&& DCDDL AC#APG20,8,C                                                     
         DC    AL1(9,11)           BONUS                                        
         DCDDL AC#APG21,8,C                                                     
         DC    AL1(9,12)           BENEFITS                                     
         DCDDL AC#APG22,8,C                                                     
         DC    AL1(9,13)           FEES                                         
         DCDDL AC#APG23,8,C                                                     
         DC    AL1(9,14)           DEBIT                                        
         DCDDL AC#APG24,8,C                                                     
         DC    AL1(9,15)           CREDIT                                       
         DCDDL AC#APG25,8,C                                                     
         DC    AL1(9,16)           BALANCE                                      
         DCDDL AC#APG26,8,C                                                     
         DC    AL1(9,17)           DISCOUNT                                     
         DCDDL AC#APG27,8,C                                                     
         DC    AL1(9,18)           COMM.                                        
         DCDDL AC#APG28,8,C                                                     
         DC    AL1(9,19)           BILLING                                      
         DCDDL AC#APG29,8,C                                                     
         DC    AL1(9,20)           BASIC                                        
         DCDDL AC#APG30,8,C                                                     
         DC    AL1(9,21)           TOT.SAL                                      
         DCDDL AC#APG31,8,C                                                     
         DC    AL1(9,22)           NETBILL                                      
         DCDDL AC#APG32,8,C                                                     
         DC    AL1(9,23)           DEBIT                                        
         DCDDL AC#APG33,8,C                                                     
         DC    AL1(9,24)           CREDIT                                       
         DCDDL AC#APG34,8,C                                                     
         DC    AL1(9,25)           BALANCE                                      
         DCDDL AC#APG35,8,C                                                     
         DC    AL1(9,26)           ORDERED                                      
         DCDDL AC#APG36,8,C                                                     
         DC    AL1(9,27)           BILLABLE                                     
         DCDDL AC#APG37,8,C                                                     
         DC    AL1(9,28)           CLEARED                                      
         DCDDL AC#APG38,8,C                                                     
         DC    AL1(9,29)           UNCLEAR                                      
         DCDDL AC#APG39,8,C                                                     
         DC    AL1(9,30)           ADMIN.                                       
         DCDDL AC#APG40,8,C                                                     
         DC    AL1(9,40)           NETSAL                                       
         DCDDL AC#APG41,8,C                                                     
         DC    AL1(9,41)           BALFWD                                       
         DCDDL AC#APG42,8,C                                                     
         DC    AL1(9,42)           BALFWD                                       
         DCDDL AC#APG43,8,C                                                     
         DC    AL1(9,43)           ORIG EST                                     
         DCDDL AC#APG44,8,C                                                     
         DC    AL1(9,44)           ORIG EST                                     
         DCDDL AC#APG45,8,C                                                     
         DC    AL1(9,45)           CURR EST                                     
         DCDDL AC#APG46,8,C                                                     
         DC    AL1(9,46)           CURR EST                                     
         DCDDL AC#APG47,8,C                                                     
         DC    AL1(9,47)           PREV EST                                     
         DCDDL AC#APG48,8,C                                                     
         DC    AL1(9,48)           PREV EST                                     
         DCDDL AC#APG49,8,C                                                     
*&&US                                                                           
         DC    AL1(9,49)           GROSS                                        
         DCDDL AC#APG50,8,C                                                     
         DC    AL1(9,50)           GROSS                                        
         DCDDL AC#APG51,8,C                                                     
         DC    AL1(9,51)           NET                                          
         DCDDL AC#APG52,8,C                                                     
         DC    AL1(9,52)           NET                                          
         DCDDL AC#APG53,8,C                                                     
         DC    AL1(9,53)           ADJ DR                                       
         DCDDL AC#APG54,8,C                                                     
*&&                                                                             
*&&UK                                                                           
         DC    AL1(9,49)           V.A.T.                                       
         DCDDL AC#APG55,8,C                                                     
         DC    AL1(9,50)           V.A.T.                                       
         DCDDL AC#APG56,8,C                                                     
         DC    AL1(9,51)           CHG HRS                                      
         DCDDL AC#APG57,8,C                                                     
         DC    AL1(9,52)           CHG AMT                                      
         DCDDL AC#APG58,8,C                                                     
         DC    AL1(9,53)           CHG AMT                                      
         DCDDL AC#APG59,8,C                                                     
*&&                                                                             
         DC    AL1(9,54)           ADJ DR                                       
         DCDDL AC#APG60,8,C                                                     
         DC    AL1(9,55)           ADJ PCT                                      
         DCDDL AC#APG61,8,C                                                     
         DC    AL1(9,56)           ADJ CR                                       
         DCDDL AC#APG62,8,C                                                     
         DC    AL1(9,57)           ADJ CR                                       
         DCDDL AC#APG63,8,C                                                     
         DC    AL1(9,58)           FEE ADJ                                      
         DCDDL AC#APG64,8,C                                                     
         DC    AL1(9,59)           MW RATIO                                     
         DCDDL AC#APG65,8,C                                                     
         DC    AL1(9,60)           TYPE1%                                       
         DCDDL AC#APG66,8,C                                                     
         DC    AL1(9,61)           TOTHRS                                       
         DCDDL AC#APG67,8,C                                                     
         DC    AL1(9,62)           STFHRS                                       
         DCDDL AC#APG68,8,C                                                     
         DC    AL1(9,63)           CLTHRS                                       
         DCDDL AC#APG69,8,C                                                     
         DC    AL1(9,64)           RATE                                         
         DCDDL AC#APG70,8,C                                                     
         DC    AL1(9,65)           BILL RATE                                    
         DCDDL AC#APG71,8,C                                                     
         DC    AL1(9,66)           ADJ HRS                                      
         DCDDL AC#APG72,8,C                                                     
         DC    AL1(9,67)           MISS HRS                                     
         DCDDL AC#APG73,8,C                                                     
         DC    AL1(9,68)           SALARY                                       
         DCDDL AC#APG74,8,C                                                     
         DC    AL1(9,69)           BENEFITS                                     
         DCDDL AC#APG75,8,C                                                     
         DC    AL1(9,70)           PENSION                                      
         DCDDL AC#APG76,8,C                                                     
         DC    AL1(9,71)           TOT SAL                                      
         DCDDL AC#APG77,8,C                                                     
         DC    AL1(9,KWMTS)        MISS T/S                                     
         DCDDL AC#APG78,8,C                                                     
         DC    X'FF'                                                            
         TITLE 'TABLES - ACCUMULATOR LINE STATUS'                               
***********************************************************************         
* TABLE OF ACCUMULATOR LINE STATUS                                    *         
***********************************************************************         
         SPACE 1                                                                
LNSTL    DS    0C                                                               
*                                                                               
         ORG   LNSTL+LNZRO                                                      
L00      DC    AL1(0)                                                           
         ORG   LNSTL+LNDRS                                                      
L01      DC    AL1(LSTRN+LSSUB+LSBUF)                                           
         ORG   LNSTL+LNCRS                                                      
L02      DC    AL1(LSTRN+LSSUB+LSBUF)                                           
         ORG   LNSTL+LNCMM                                                      
L03      DC    AL1(LSTRN+LSSUB+LSBUF)                                           
         ORG   LNSTL+LNCSD                                                      
L04      DC    AL1(LSTRN+LSSUB+LSBUF)                                           
         ORG   LNSTL+LNFEE                                                      
L05      DC    AL1(LSSUB+LSBUF)                                                 
         ORG   LNSTL+LNGRB                                                      
L06      DC    AL1(LSTRN+LSSUB+LSBUF)                                           
         ORG   LNSTL+LNHRS                                                      
L07      DC    AL1(LSSUB+LSBUF)                                                 
         ORG   LNSTL+LNNET                                                      
L08      DC    AL1(LSSUB+LSBUF)                                                 
         ORG   LNSTL+LNBEN                                                      
L09      DC    AL1(LSSUB+LSBUF)                                                 
         ORG   LNSTL+LNADM                                                      
L10      DC    AL1(LSSUB+LSBUF)                                                 
         ORG   LNSTL+LNORD                                                      
L11      DC    AL1(LSSUB+LSBUF)                                                 
         ORG   LNSTL+LNCLD                                                      
L12      DC    AL1(LSSUB+LSBUF)                                                 
*&&US                                                                           
         ORG   LNSTL+LNEXG                                                      
L13      DC    AL1(LSTRN+LSBUF)                                                 
         ORG   LNSTL+LNEXN                                                      
L14      DC    AL1(LSTRN+LSBUF)                                                 
         ORG   LNSTL+LNAJD                                                      
L15      DC    AL1(LSSUB+LSBUF)                                                 
*&&                                                                             
*&&UK                                                                           
         ORG   LNSTL+LNVAT                                                      
L13      DC    AL1(LSTRN+LSBUF)                                                 
         ORG   LNSTL+LNCHR                                                      
L14      DC    AL1(LSTRN+LSBUF)                                                 
         ORG   LNSTL+LNCAM                                                      
L15      DC    AL1(LSTRN+LSBUF)                                                 
*&&                                                                             
         ORG   LNSTL+LNGPC                                                      
L16      DC    AL1(LSACC)                                                       
         ORG   LNSTL+LNCLT                                                      
L17      DC    AL1(LSACC)                                                       
         ORG   LNSTL+LNTLT                                                      
L18      DC    AL1(LSACC)                                                       
         ORG   LNSTL+LNTLV                                                      
L19      DC    AL1(LSACC)                                                       
         ORG   LNSTL+LNTLP                                                      
L20      DC    AL1(LSACC)                                                       
         ORG   LNSTL+LNSAL                                                      
L21      DC    AL1(LSACC)                                                       
*&&US                                                                           
         ORG   LNSTL+LNOVT                                                      
L22      DC    AL1(LSACC)                                                       
         ORG   LNSTL+LNTMP                                                      
L23      DC    AL1(LSACC)                                                       
*&&                                                                             
*&&UK                                                                           
         ORG   LNSTL+LNBVT         BVAT                                         
L22      DC    AL1(LSSUB+LSBUF)                                                 
         ORG   LNSTL+LNUVT         UVAT                                         
L23      DC    AL1(LSSUB+LSBUF)                                                 
*&&                                                                             
         ORG   LNSTL+LNBON                                                      
L24      DC    AL1(LSACC)                                                       
         ORG   LNSTL+LNAJR                                                      
L25      DC    AL1(LSSUB+LSBUF)                                                 
         ORG   LNSTL+LNFAJ                                                      
L26      DC    AL1(LSACC+LSSUB+LSBUF)                                           
         ORG   LNSTL+LNWKW                                                      
L27      DC    AL1(LSACC)                                                       
         ORG   LNSTL+LNWKP                                                      
L28      DC    AL1(LSACC)                                                       
         ORG   LNSTL+LNDID                                                      
L29      DC    AL1(LSACC)                                                       
         ORG   LNSTL+LNC30                                                      
L30      DC    AL1(LSYES)                                                       
         ORG   LNSTL+LNS31                                                      
L31      DC    AL1(LSYES)                                                       
         ORG   LNSTL+LNS32                                                      
L32      DC    AL1(LSYES)                                                       
         ORG   LNSTL+LNS33                                                      
L33      DC    AL1(LSYES)                                                       
         ORG   LNSTL+LNS34                                                      
L34      DC    AL1(LSSUB+LSBUF)                                                 
         ORG   LNSTL+LNS35                                                      
L35      DC    AL1(LSSUB+LSBUF)                                                 
         ORG   LNSTL+LNS36                                                      
L36      DC    AL1(LSSUB+LSBUF)                                                 
         ORG   LNSTL+LNSWH                                                      
L37      DC    AL1(LSTRN+LSSUB+LSNOT+LSBUF)                                     
         ORG   LNSTL+LNBCW                                                      
L38      DC    AL1(LSYES)                                                       
         ORG   LNSTL+LNHOL                                                      
L39      DC    AL1(LSYES)                                                       
         ORG   LNSTL+LNS40                                                      
L40      DC    AL1(LSYES)                                                       
         ORG   LNSTL+LNBG1                                                      
L41      DC    AL1(LSBUD)                                                       
         ORG   LNSTL+LNBG2                                                      
L42      DC    AL1(LSBUD)                                                       
         ORG   LNSTL+LNBG3                                                      
L43      DC    AL1(LSBUD)                                                       
         ORG   LNSTL+LNBG4                                                      
L44      DC    AL1(LSBUD)                                                       
         ORG   LNSTL+LNBG5                                                      
L45      DC    AL1(LSBUD)                                                       
         ORG   LNSTL+LNBG6                                                      
L46      DC    AL1(LSBUD)                                                       
         ORG   LNSTL+LNBG7                                                      
L47      DC    AL1(LSBUD)                                                       
         ORG   LNSTL+LNBG8                                                      
L48      DC    AL1(LSBUD)                                                       
         ORG   LNSTL+LNBG9                                                      
L49      DC    AL1(LSBUD)                                                       
         ORG   LNSTL+LNBGA                                                      
L50      DC    AL1(LSBUD)                                                       
         ORG   LNSTL+LNBGB                                                      
L51      DC    AL1(LSBUD)                                                       
         ORG   LNSTL+LNBGC                                                      
L52      DC    AL1(LSBUD)                                                       
         ORG   LNSTL+LNBGD                                                      
L53      DC    AL1(LSBUD)                                                       
         ORG   LNSTL+LNBGE                                                      
L54      DC    AL1(LSBUD)                                                       
         ORG   LNSTL+LNBGF                                                      
L55      DC    AL1(LSBUD)                                                       
         ORG   LNSTL+LNBLR                                                      
L56      DC    AL1(LSACC)                                                       
         ORG   LNSTL+LNBRH                                                      
L57      DC    AL1(LSSUB+LSBUF)                                                 
         ORG   LNSTL+LNADH            ADJUSTED HOURS                            
L58      DC    AL1(LSTRN+LSBUF)                                                 
         ORG   LNSTL+LNMSH                                                      
L59      DC    AL1(LSTRN+LSBUF)              MISSING HOURS                      
         ORG   LNSTL+LNASL                                                      
L60      DC    AL1(LSSUB+LSBUF)                                                 
         ORG   LNSTL+LNABN                                                      
L61      DC    AL1(LSSUB+LSBUF)                                                 
         ORG   LNSTL+LNAPN                                                      
L62      DC    AL1(LSSUB+LSBUF)                                                 
         ORG   LNSTL+LNTHR                                                      
L63      DC    AL1(LSSUB+LSTRN+LSBUF)        TRANSACTION HOURS                  
         ORG   LNSTL+LNMTS                                                      
L64      DC    AL1(LSACC)                    MISSING T/S                        
         ORG   LNSTL+LNSTR                                                      
L65      DC    AL1(LSACC)                    STANDARD RATES                     
         ORG   LNSTL+LNST$                                                      
L66      DC    AL1(LSTRN+LSBUF)              STANDARD DOLLARS                   
         ORG   LNSTL+LNPSR                                                      
L67      DC    AL1(LSACC)                    SALARY RATE                        
         ORG   LNSTL+LNPS$                                                      
L68      DC    AL1(LSSUB+LSBUF)              SALARY DOLLARS                     
         ORG   LNSTL+LNPBR                                                      
L69      DC    AL1(LSACC)                    BENEFIT RATE                       
         ORG   LNSTL+LNPB$                                                      
L70      DC    AL1(LSSUB+LSBUF)              BENEFIT DOLLARS                    
         ORG   LNSTL+LNPPR                                                      
L71      DC    AL1(LSACC)                    PENSION RATE                       
         ORG   LNSTL+LNPP$                                                      
L72      DC    AL1(LSSUB+LSBUF)              PENSION DOLLARS                    
         ORG   LNSTL+LNPTR                                                      
L73      DC    AL1(LSACC)                    TOTAL RATE                         
         ORG   LNSTL+LNPT$                                                      
L74      DC    AL1(LSSUB+LSBUF)              TOTAL DOLLARS                      
         ORG   LNSTL+LNTO$                                                      
L75      DC    AL1(LSSUB+LSBUF)              TOTAL $                            
         ORG   LNSTL+LNMS$                                                      
L76      DC    AL1(LSSUB+LSBUF)              MISSING T/S $                      
         ORG   LNSTL+LNAD$                                                      
L77      DC    AL1(LSSUB+LSBUF)              ADJUST T/S $                       
         ORG   LNSTL+LNCU$                                                      
L78      DC    AL1(LSSUB+LSBUF)              CURRENT $                          
         ORG   LNSTL+LNCUR                                                      
L79      DC    AL1(LSSUB+LSBUF)              CURRENT RATE                       
         ORG   LNSTL+LNSPBT$                                                    
L80      DC    AL1(LSSUB+LSBUF)              SAL, PEN OR BEN TOTAL $            
         ORG   LNSTL+LNSPBM$                                                    
L81      DC    AL1(LSSUB+LSBUF)              SAL, PEN OR BEN MISSING $          
         ORG   LNSTL+LNSPBA$                                                    
L82      DC    AL1(LSSUB+LSBUF)              SAL, PEN OR BEN ADJUST $           
         ORG   LNSTL+LNSPBC$                                                    
L83      DC    AL1(LSSUB+LSBUF)              SAL, PEN OR BEN CURRENT $          
         ORG   LNSTL+LNSPBR                                                     
L84      DC    AL1(LSSUB+LSBUF)              SAL/PEN/BEN CURRENT RATE           
         ORG   LNSTL+LNHIST                                                     
*MN                                                                             
L85      DC    AL1(LSACC)                                                       
         ORG   LNSTL+LNMX                    END OF TABLE                       
*MN                                                                             
         DC    X'00'                                                            
         TITLE 'TABLES - OTHER'                                                 
***********************************************************************         
* MEDIA NAME TABLE                                                    *         
***********************************************************************         
         SPACE 1                                                                
         DC    C'**MEDN*'                                                       
MEDN     DS    0CL17               MEDIA NAME TABLE                             
         DC    C'SR',CL15'RADIO'                                                
         DC    C'ST',CL15'TELEVISION'                                           
         DC    C'SM',CL15'MISC TV'                                              
         DC    C'PO',CL15'OUTDOOR'                                              
         DC    C'PN',CL15'NEWSPAPER'                                            
         DC    C'PS',CL15'MISC'                                                 
         DC    C'PL',CL15'LOCAL EXTRA'                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
***********************************************************************         
* TABLE OF ALLOCATION RUN DATES                                       *         
***********************************************************************         
         SPACE 1                                                                
AGYADTE  DC    XL(NAC*2)'00' AGENCY ALLOCATION DATES                            
OFFADTE  DC    XL(NAC*2)'00' OFFICE ALLOCATION DATES                            
         EJECT                                                                  
***********************************************************************         
* SAVE TABLE FOR TRANSACTION HOURS, MISSING HOURS, ADJ HOURS         *          
***********************************************************************         
         SPACE 1                                                                
         DS    0F  FROM       TO                                                
HRSTAB   DC    AL1(LNTHR),AL1(LNS34)    TRANSACTION HOURS                       
         DC    AL1(LNMSH),AL1(LNS35)    MISSING HOURS                           
         DC    AL1(LNADH),AL1(LNS36)    ADJUSTED HOURS                          
         DC    X'FFFF'                                                          
*                                                                               
         SPACE 2                                                                
***********************************************************************         
* SALARY, PENSION, BENEFIT LINE COMPUTATION TABLE                    *          
***********************************************************************         
         SPACE 1                                                                
SPBTAB   DS    0F                                                               
*                                  TOTAL OF SAL, PEN & BEN                      
         DC    C'1'                BUCKET TYPE                                  
         DC    XL2'00'             SPARE                                        
         DC    AL1(LNTO$)          LINE NUMBER FOR TOTAL $                      
         DC    AL1(LNAD$,LNCUR,LNMS$,LNCU$) LINE NUMBERS                        
         DC    AL4(LNPTR*WLN)      RATE                                         
         DC    AL4(LNS36*WLN)      ADJUSTED HOURS                               
         DC    AL4(LNAD$*WLN)      ADJUSTED DOLLARS                             
         DC    AL4(NAC)            NUMBER OF ACCUM COLUMNS                      
         DC    AL4(LNS34*WLN)      TRANSACTION HOURS                            
         DC    AL4(LNTO$*WLN)      TOTAL DOLLARS                                
         DC    AL4(LNCUR*WLN)      CURRENT RATE                                 
         DC    AL4(LNS35*WLN)      MISSING HOURS                                
         DC    AL4(LNMS$*WLN)      MISSING DOLLARS                              
         DC    AL4(LNCU$*WLN)      CURRENT DOLLARS                              
*                                                                               
*                                  SALARY                                       
         DC    C'1'                BUCKET TYPE                                  
         DC    XL2'00'             SPARE                                        
         DC    AL1(LNSPBT$)        LINE NUMBER FOR TOTAL $                      
         DC    AL1(LNSPBA$,LNSPBR,LNSPBM$,LNSPBC$) LINE NUMBERS                 
         DC    AL4(LNPSR*WLN)      RATE                                         
         DC    AL4(LNS36*WLN)      ADJUSTED HOURS                               
         DC    AL4(LNSPBA$*WLN)    ADJUSTED DOLLARS                             
         DC    AL4(NAC)            NUMBER OF ACCUM COLUMNS                      
         DC    AL4(LNS34*WLN)      TRANSACTION HOURS                            
         DC    AL4(LNSPBT$*WLN)    TOTAL DOLLARS                                
         DC    AL4(LNSPBR*WLN)     CURRENT RATE                                 
         DC    AL4(LNS35*WLN)      MISSING HOURS                                
         DC    AL4(LNSPBM$*WLN)    MISSING DOLLARS                              
         DC    AL4(LNSPBC$*WLN)    CURRENT DOLLARS                              
*                                                                               
*                                  PENSION                                      
         DC    C'2'                BUCKET TYPE                                  
         DC    XL2'00'             SPARE                                        
         DC    AL1(LNSPBT$)        LINE NUMBER FOR TOTAL $                      
         DC    AL1(LNSPBA$,LNSPBR,LNSPBM$,LNSPBC$) LINE NUMBERS                 
         DC    AL4(LNPPR*WLN)      RATE                                         
         DC    AL4(LNS36*WLN)      ADJUSTED HOURS                               
         DC    AL4(LNSPBA$*WLN)    ADJUSTED DOLLARS                             
         DC    AL4(NAC)            NUMBER OF ACCUM COLUMNS                      
         DC    AL4(LNS34*WLN)      TRANSACTION HOURS                            
         DC    AL4(LNSPBT$*WLN)    TOTAL DOLLARS                                
         DC    AL4(LNSPBR*WLN)     CURRENT RATE                                 
         DC    AL4(LNS35*WLN)      MISSING HOURS                                
         DC    AL4(LNSPBM$*WLN)    MISSING DOLLARS                              
         DC    AL4(LNSPBC$*WLN)    CURRENT DOLLARS                              
*                                                                               
*                                  BENEFIT                                      
         DC    C'3'                BUCKET TYPE                                  
         DC    XL2'00'             SPARE                                        
         DC    AL1(LNSPBT$)        LINE NUMBER FOR TOTAL $                      
         DC    AL1(LNSPBA$,LNSPBR,LNSPBM$,LNSPBC$) LINE NUMBERS                 
         DC    AL4(LNPBR*WLN)      RATE                                         
         DC    AL4(LNS36*WLN)      ADJUSTED HOURS                               
         DC    AL4(LNSPBA$*WLN)    ADJUSTED DOLLARS                             
         DC    AL4(NAC)            NUMBER OF ACCUM COLUMNS                      
         DC    AL4(LNS34*WLN)      TRANSACTION HOURS                            
         DC    AL4(LNSPBT$*WLN)    TOTAL DOLLARS                                
         DC    AL4(LNSPBR*WLN)     CURRENT RATE                                 
         DC    AL4(LNS35*WLN)      MISSING HOURS                                
         DC    AL4(LNSPBM$*WLN)    MISSING DOLLARS                              
         DC    AL4(LNSPBC$*WLN)    CURRENT DOLLARS                              
         DC    X'FF'                                                            
       ++INCLUDE ACAPGEQU                                                       
       ++INCLUDE ACAPGDSECT                                                     
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* ACPERCALLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACPERCALLD                                                     
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023ACREPM206 01/28/13'                                      
         END                                                                    
