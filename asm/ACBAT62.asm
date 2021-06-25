*          DATA SET ACBAT62    AT LEVEL 031 AS OF 12/28/17                      
*PHASE T61B62A                                                                  
BAT62    TITLE '- BATCH PROGRAM POSTING RECORD HANDLING'                        
BAT62    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BA62**,R8,R7,R6,RR=RE                                        
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA                                                          
         USING BSVALS,R5                                                        
         L     RC,AOVERWRK                                                      
         ST    RE,BORELO                                                        
         L     R1,AMIXNTRY                                                      
         SRL   RF,32-8                                                          
         LTR   RF,RF                                                            
         BNZ   PSTNTRY                                                          
         ICM   RF,1,MIXROUT-MIXTABD(R1)                                         
         CLM   RF,1,=AL1(PSTROUTM)                                              
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     PSTROUTS(RF)                                                     
*                                                                               
PSTROUTS DS    0XL4                                                             
         B     PSTTXD                                                           
         B     PSTLST                                                           
PSTROUTM EQU   (*-PSTROUTS)/L'PSTROUTS                                          
         EJECT                                                                  
***********************************************************************         
* RETURN TO POINT AFTER NTRSES                                        *         
***********************************************************************         
         SPACE 1                                                                
PSTNTRY  BCTR  RF,0                                                             
         CLM   RF,1,=AL1(PSTNTRYM)                                              
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     PSTNTRYS(RF)                                                     
*                                                                               
PSTNTRYS DS    0XL4                                                             
PSTLST#1 EQU   1                                                                
         B     PSTLSTR1                                                         
PSTNTRYM EQU   (*-PSTNTRYS)/L'PSTNTRYS                                          
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE MAIN TRANSACTION DETAILS                                *         
***********************************************************************         
         SPACE 1                                                                
         USING PDWORKD,RC                                                       
PSTTXD   TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BZ    ERRINVAS                                                         
         LA    R2,BCBATCUR         R2=A(LSTTAB ENTRY)                           
         USING LSTTABD,R2                                                       
         GOTO1 AOVRSCR,BCPARM,('PTXNSCRN',BASOLY1H)                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,PTXBAT           BATCH DETAILS                                
         LA    R1,L'LSTBBREF-1                                                  
         LA    RF,LSTBBREF                                                      
         BAS   RE,PSTBDCA                                                       
         LA    R1,L'LSTBNAME-1                                                  
         LA    RF,LSTBNAME                                                      
         BAS   RE,PSTBDCA                                                       
         CURED LSTBBTYP,(3,(R3)),0,ALIGN=LEFT                                   
         LA    R1,3-1                                                           
         BAS   RE,PSTBDCB                                                       
         MVC   BOFULL1(L'LSTBMOSP),LSTBMOSP                                     
         MVI   BOFULL1+L'LSTBMOSP,1                                             
         GOTO1 VDATCON,BOPARM,(1,BOFULL1),(9,(R3))                              
*                                                                               
         LA    R2,CSLSTCUR                                                      
         LA    R3,IOKEY            READ ACCOUNT RECORD                          
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCULA(L'ACTKCULA),LSTPACT                                     
         GOTO1 AIO,IORD+IOACCMST+IO1                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   PDSUNDRY,C'N'       TEST FOR A SUNDRY CREDITOR A/C               
*&&UK                                                                           
         BAS   RE,PSTSCD                                                        
         BNE   *+8                                                              
         MVI   PDSUNDRY,C'Y'                                                    
*&&                                                                             
         L     R3,AIO1             SET DEFAULT A/C DETAILS                      
         USING ACTRECD,R3                                                       
         GOTO1 PSTNAME,ACTRFST                                                  
         MVC   PTXACCN(L'PTXACCN),BOWORK1 ACCOUNT NAME                          
         MVC   PTXACC(L'PTXACC),LSTPACT+1 ACCOUNT                               
         MVC   PDUNLD(L'PDUNLD),LSTPACT+1 UNIT/LEDGER                           
*                                                                               
         LA    R3,IOKEY            READ CONTRA HEADER RECORD                    
         USING CHDRECD,R3                                                       
         MVC   CHDKEY,BCSPACES                                                  
         MVC   CHDKCULA,LSTPACT                                                 
         MVC   CHDKOFF,LSTPKOF                                                  
         MVC   CHDKCULC,LSTPCAC                                                 
         XC    CHDKNULL,CHDKNULL                                                
         GOTO1 AIO,IORD+IOACCMST+IO1                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO1             SET DEFAULT CONTRA A/C DETAILS               
         GOTO1 PSTNAME,CHDRFST                                                  
         MVC   PTXCACN(L'PTXCACN),BOWORK1                                       
         LA    R0,L'PTXCAC         ERASE LEADING SPACES FROM CONTRA A/C         
         MVC   PTXCAC,LSTPCAC+(TRNKCUNT-TRNKCULC)                               
         CLI   PTXCAC,C' '                                                      
         BH    *+18                                                             
         MVC   PTXCAC(L'PTXCAC-1),PTXCAC+1                                      
         MVI   PTXCAC+(L'PTXCAC-1),C' '                                         
         BCT   R0,*-18                                                          
*&&UK                                                                           
         LA    R3,IOKEY            IS CONTRA A/C A SUNDRY CREDITOR?             
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCULA(L'ACTKCULA),LSTPCAC                                     
         GOTO1 AIO,IORD+IOACCMST+IO1                                            
         BNE   PSTTXD02                                                         
         BAS   RE,PSTSCD                                                        
         BNE   PSTTXD02                                                         
         L     R3,AIO1             YES - OVERRIDE CONTRA A/C NAME WITH          
         GOTO1 PSTNAME,ACTRFST           DEFAULT S/CRD A/C NAME                 
         MVC   PTXCACN(L'PTXCACN),BOWORK1                                       
*&&                                                                             
PSTTXD02 L     R3,AIO1             READ TRANSACTION RECORD                      
         USING TRNRECD,R3                                                       
         MVC   IODAOVER,LSTTDA                                                  
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TRNKREF,BCSPACES    TRANSACTION REFERENCE                        
         BE    *+10                                                             
         MVC   PTXREF(L'TRNKREF),TRNKREF                                        
PSTTXD04 GOTO1 VDATCON,BCPARM,(1,TRNKDATE),(17,PTXDATE)                         
*&&US*&& CURED LSTPAMT,(L'PTXAMT,PTXAMT),2,MINUS=YES,ALIGN=LEFT                 
*&&UK*&& CURED LSTPAMT,(L'PTXAMT,PTXAMT),2,FLOAT=-,ALIGN=LEFT                   
         LA    RF,PTXAMT                                                        
         AR    RF,R0                                                            
         MVC   1(L'BC@DR-1,RF),BC@DR                                            
         TM    LSTPSTAT,TRNSDR                                                  
         BNZ   *+10                                                             
         MVC   1(L'BC@CR-1,RF),BC@CR                                            
*                                                                               
         CLC   TRNKUNT(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         BNE   PSTTXD06            WORK CODES ON PRODUCTION                     
         MVC   PTXWC(L'PTXWC),BCSPACES                                          
         LH    RF,=Y(LC@WC-TWAD)                                                
         LA    RF,TWAD(RF)                                                      
         MVC   PTXWC(L'LC@WC),0(RF)                                             
         OC    LSTPOFF,LSTPOFF                                                  
         BZ    PSTTXD12                                                         
         MVC   PTXWOC(L'PTXWOC),LSTPOFF                                         
         GOTO1 AGETWRK,LSTPOFF                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PTXWOCN(L'WCODESC),BOWORK1                                       
         B     PSTTXD12                                                         
*                                                                               
PSTTXD06 TM    BCCPYST1,CPYSOROE   TEST COMPANY ON OFFICES                      
         BZ    PSTTXD12                                                         
         OC    LSTPOFF,LSTPOFF                                                  
         BZ    PSTTXD12                                                         
         MVC   PTXWOC(L'PTXWOC),LSTPOFF  OFFICE CODE                            
         TM    BCCPYST4,CPYSOFF2   OLD OR NEW OFFICES?                          
         BZ    PSTTXD10                                                         
*                                                                               
         LA    RF,IOKEY            READ OFFICE RECORD                           
         USING OFFRECD,RF          (NEW OFFICES)                                
         MVC   OFFKEY,BCSPACES                                                  
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUABIN                                                   
         MVC   OFFKOFF,LSTPOFF                                                  
         GOTO1 AIO,IORD+IOACCMST+IO2                                            
         BNE   PSTTXD12                                                         
         L     RF,AIO2                                                          
         GOTO1 PSTNAME,OFFRFST                                                  
         MVC   PTXWOCN(L'PTXWOCN),BOWORK1                                       
         B     PSTTXD12                                                         
*                                                                               
PSTTXD10 LA    RF,IOKEY            READ DEPARTMENT RECORD                       
         USING ACTRECD,RF          (OLD OFFICES)                                
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVI   ACTKUNT,C'2'                                                     
         MVI   ACTKLDG,C'D'                                                     
         MVC   ACTKACT(1),LSTPOFF                                               
         GOTO1 AIO,IORD+IOACCMST+IO2                                            
         BNE   PSTTXD12                                                         
         L     RF,AIO2                                                          
         GOTO1 PSTNAME,ACTRFST                                                  
         MVC   PTXWOCN(L'PTXWOCN),BOWORK1                                       
         DROP  RF                                                               
*                                                                               
PSTTXD12 CLI   PDSUNDRY,C'Y'       IS THIS A SUNDRY CREDITOR?                   
         BNE   PSTTXD13                                                         
         GOTO1 PSTNAME,TRNRFST     YES - SEARCH FOR S/C NAME ON TX RECD         
         CLC   BOWORK1,BCSPACES                                                 
         BNH   PSTTXD13                                                         
         MVC   PTXACCN(L'PTXACCN),BOWORK1  OVERRIDE A/C NAME IF FOUND           
PSTTXD13 XC    BOBYTE2,BOBYTE2                                                  
         LA    R2,TRNRFST          READ TRANSACTION RECORD ELEMEMTS             
PSTTXD14 CLI   0(R2),0                                                          
         BE    PSTTXD22                                                         
         CLI   0(R2),TRNELQ        TRANSACTION ELEMENT                          
         BNE   *+14                                                             
         MVC   PDBATTYP,TRNTYPE-TRNELD(R2)                                      
         B     PSTTXD16                                                         
         CLI   0(R2),OTHELQ        OTHER ELEMENT                                
         BE    PSTTXD18                                                         
         CLI   0(R2),FFTELQ        FREE FORM TEXT ELEMENT                       
         BE    PSTTXD19                                                         
PSTTXD16 XR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     PSTTXD14                                                         
*                                                                               
         USING OTHELD,R2                                                        
PSTTXD18 LH    RF,=Y(LC@SUBR-TWAD)                                              
         LA    RF,TWAD(RF)                                                      
         MVC   PTXSUBT(L'LC@SUBR),0(RF)                                         
         MVC   PTXSUB(9),OTHNUM                                                 
         CLI   OTHLN,15            FIND SPECIAL PRODUCT/JOB                     
         BNE   PSTTXD16                                                         
         CLC   OTHNUM+3(3),BCSPACES                                             
         BNE   PSTTXD16                                                         
         MVC   PTXSUB(12),OTHNUM   PRODUCT(6),JOB+6(6)                          
         B     PSTTXD16                                                         
*                                                                               
         USING FFTELD,R2                                                        
PSTTXD19 CLI   FFTTYPE,FFTTSCDN    SUNDRY CREDITOR NAME                         
         BNE   PSTTXD20                                                         
         XR    RF,RF                                                            
         ICM   RF,1,FFTDLEN                                                     
         BZ    PSTTXD16                                                         
         MVC   PTXCACN,BCSPACES    OVERRIDE CONTRA A/C NAME                     
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   PTXCACN(0),FFTDATA                                               
         B     PSTTXD16                                                         
PSTTXD20 CLI   FFTTYPE,FFTTTAXI    DUTCH TAX INDICATOR                          
         BNE   PSTTXD21                                                         
         XR    RF,RF                                                            
         ICM   RF,1,FFTDLEN                                                     
         BZ    PSTTXD16                                                         
         MVC   BOBYTE2,FFTDATA                                                  
         B     PSTTXD16                                                         
PSTTXD21 CLI   FFTTYPE,FFTTKREF    ORIGINAL KEY REFERENCE                       
         BNE   *+14                                                             
         MVC   PTXREF(L'TRNKREF),FFTDATA                                        
         B     PSTTXD16                                                         
         CLI   FFTTYPE,FFTTINVN    SUPPLIER INVOICE NUMBER                      
         BNE   PSTTXD16                                                         
         LA    R0,L'PTXREF         R0=GET MAX FIELD LENGTH                      
         XR    RF,RF                                                            
         ICM   RF,1,FFTDLEN        RF=DATA LENGTH                               
         BNZ   *+6                 TEST DATA LENGTH SET                         
         LR    RF,R0               NO, USE FIELD LENGTH                         
         CR    RF,R0               TEST DATA LARGER THAN FIELD                  
         BNH   *+6                 NO,                                          
         LR    RF,R0               YES, USE FIELD LENGTH                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PTXREF(0),FFTDATA                                                
         B     PSTTXD16                                                         
         DROP  R2                                                               
*                                                                               
PSTTXD22 LA    R0,L'PTXSTAT        STATUS                                       
         STH   R0,BOHALF1                                                       
         LA    R2,BOWORK1                                                       
         MVI   0(R2),C' '                                                       
         MVC   1(L'BOWORK1+L'BOWORK2-1,R2),0(R2)                                
         XC    BOFLAG1,BOFLAG1                                                  
*                                                                               
         CLC   BCCPYPRD,CSLSTCUR+(LSTPACT-LSTTABD+(ACTKUNT-ACTRECD))            
         BNE   PSTTXD26                                                         
         CLI   CSBTYP,BT70         INVOICE BATCH                                
         BNE   PSTTXD23                                                         
         CLC   CSBTYP,PDBATTYP     SPLIT INVOICE BATCH                          
         BNE   *+16                                                             
         TM    CSLSTCUR+(LSTPIND1-LSTTABD),LSTP1JEX  ONLY MAIN IS BLB           
         BO    PSTTXD25                                                         
         B     PSTTXD24                                                         
         CLI   PDBATTYP,BT01                                                    
         BE    PSTTXD25                                                         
         B     PSTTXD24                                                         
PSTTXD23 CLI   CSBTYP,BT71         CASH BATCH                                   
         BNE   PSTTXD26                                                         
         CLI   PDBATTYP,BT03                                                    
         BE    PSTTXD25                                                         
PSTTXD24 LA    R0,L'LC@NBLB-1                                                   
         LH    RF,=Y(LC@NBLB-TWAD)                                              
         B     *+12                                                             
PSTTXD25 LA    R0,L'LC@BLB-1                                                    
         LH    RF,=Y(LC@BLB-TWAD)                                               
         LA    RF,TWAD(RF)                                                      
         GOTO1 PSTSLC,BOPARM,((R0),(RF)),0                                      
*                                                                               
PSTTXD26 TM    TRNRSTAT,TRNSDRFT   (DRAFT/LIVE)                                 
         BZ    *+16                                                             
         LA    R0,L'LC@DRAFT-1                                                  
         LH    RF,=Y(LC@DRAFT-TWAD)                                             
         B     *+12                                                             
         LA    R0,L'LC@LIVE-1                                                   
         LH    RF,=Y(LC@LIVE-TWAD)                                              
         LA    RF,TWAD(RF)                                                      
         GOTO1 PSTSLC,BOPARM,((R0),(RF)),0                                      
*                                                                               
         TM    CSLSTCUR+(LSTPTRS2-LSTTABD),TRSSACRL (ACCRUAL)                   
         BZ    PSTTXD28                                                         
         LA    R0,PDSTALEN                                                      
         LH    RF,=Y(LC@ACRL-TWAD)                                              
         LA    RF,TWAD(RF)                                                      
         GOTO1 PSTSLC,BOPARM,((R0),(RF)),0                                      
*                                                                               
PSTTXD28 TM    CSLSTCUR+(LSTPTRS2-LSTTABD),TRSSACRV (ACCRUAL REVERSAL)          
         BZ    PSTTXD30                                                         
         LA    R0,L'LC@ACRRV-1                                                  
         LH    RF,=Y(LC@ACRRV-TWAD)                                             
         LA    RF,TWAD(RF)                                                      
         GOTO1 PSTSLC,BOPARM,((R0),(RF)),0                                      
*                                                                               
PSTTXD30 TM    TRNRSTAT,TRNSREVS   (REVERSED)                                   
         BZ    PSTTXD32                                                         
         LA    R0,L'LC@RVRSD-1                                                  
         LH    RF,=Y(LC@RVRSD-TWAD)                                             
         LA    RF,TWAD(RF)                                                      
         GOTO1 PSTSLC,BOPARM,((R0),(RF)),0                                      
*                                                                               
PSTTXD32 LA    RF,VENDLIST         (USED)                                       
         TM    TRNRSTA2,TRNSUSED                                                
         BZ    PSTTXD40                                                         
         CLC   TRNKUNT(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         BNE   PSTTXD34                                                         
         LA    R0,L'LC@BLD-1                                                    
         LH    RF,=Y(LC@BLD-TWAD)  (USED - BILLED)                              
         MVI   BOFLAG1,PDDSSUB                                                  
         B     PSTTXD38                                                         
PSTTXD34 CLI   0(RF),0                                                          
         BE    PSTTXD36                                                         
         CLC   TRNKUNT(UNTLDGL),0(RF)                                           
         BE    *+12                                                             
         LA    RF,UNTLDGL(RF)                                                   
         B     PSTTXD34                                                         
         LA    R0,L'LC@PAID-1                                                   
         LH    RF,=Y(LC@PAID-TWAD) (USED - PAID)                                
         MVI   BOFLAG1,PDDSSUP                                                  
         TM    CSLSTCUR+(LSTPTRS1-LSTTABD),TRSSOFFS                             
         BZ    PSTTXD38                                                         
         LA    R0,L'LC@CTRD-1                                                   
         LH    RF,=Y(LC@CTRD-TWAD) (USED - CONTRA'D/OFFSET)                     
         MVI   BOFLAG1,PDDSSUC                                                  
         B     PSTTXD38                                                         
PSTTXD36 LA    R0,L'LC@USED-1                                                   
         LH    RF,=Y(LC@USED-TWAD)                                              
         MVI   BOFLAG1,PDDSSUU                                                  
PSTTXD38 LA    RF,TWAD(RF)                                                      
         GOTO1 PSTSLC,BOPARM,((R0),(RF)),0                                      
*                                                                               
PSTTXD40 TM    TRNRSTA2,TRNSPEEL   (PEELED)                                     
         BZ    PSTTXD42                                                         
         LA    R0,L'LC@PELED-1                                                  
         LH    RF,=Y(LC@PELED-TWAD)                                             
         LA    RF,TWAD(RF)                                                      
         GOTO1 PSTSLC,BOPARM,((R0),(RF)),0                                      
*                                                                               
PSTTXD42 TM    TRNRSTA2,TRNSGLUP   (UPDATED TO G/L)                             
         BZ    PSTTXD44                                                         
         LA    R0,L'LC@UPDGL-1                                                  
         LH    RF,=Y(LC@UPDGL-TWAD)                                             
         LA    RF,TWAD(RF)                                                      
         GOTO1 PSTSLC,BOPARM,((R0),(RF)),0                                      
*                                                                               
PSTTXD44 TM    BCCPYST4,CPYSIREG   TEST INVOICE REGISTER IN USE                 
         BZ    PSTTXD46                                                         
         TM    CSLSTCUR+(LSTPSTAT-LSTTABD),TRNSAUTH (AUTHORISED)                
         BZ    PSTTXD46                                                         
         LA    R0,L'LC@AUTHD-1                                                  
         LH    RF,=Y(LC@AUTHD-TWAD)                                             
         LA    RF,TWAD(RF)                                                      
         GOTO1 PSTSLC,BOPARM,((R0),(RF)),0                                      
*                                                                               
PSTTXD46 TM    CSLSTCUR+(LSTPSTAT-LSTTABD),TRNSURG (URGENT)                     
         BZ    PSTTXD48                                                         
         LA    R0,L'LC@URG-1                                                    
         LH    RF,=Y(LC@URG-TWAD)                                               
         LA    RF,TWAD(RF)                                                      
         GOTO1 PSTSLC,BOPARM,((R0),(RF)),0                                      
*                                                                               
PSTTXD48 TM    CSLSTCUR+(LSTPSTAT-LSTTABD),TRNSNOCM (NON-COMM)                  
         BZ    PSTTXD50                                                         
         LA    R0,L'LC@NCOM-1                                                   
         LH    RF,=Y(LC@NCOM-TWAD)                                              
         LA    RF,TWAD(RF)                                                      
         GOTO1 PSTSLC,BOPARM,((R0),(RF)),0                                      
*                                                                               
PSTTXD50 TM    CSLSTCUR+(LSTPSTAT-LSTTABD),TRNSHOLD (HELD)                      
         BZ    PSTTXD56                                                         
         CLC   TRNKUNT(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         BE    PSTTXD54                                                         
         LA    RF,VENDLIST                                                      
PSTTXD52 CLI   0(RF),0                                                          
         BE    PSTTXD56                                                         
         CLC   TRNKUNT(UNTLDGL),0(RF)                                           
         BE    *+12                                                             
         LA    RF,UNTLDGL(RF)                                                   
         B     PSTTXD52                                                         
PSTTXD54 LA    R0,L'LC@HELD-1                                                   
         LH    RF,=Y(LC@HELD-TWAD)                                              
         LA    RF,TWAD(RF)                                                      
         GOTO1 PSTSLC,BOPARM,((R0),(RF)),0                                      
*                                                                               
PSTTXD56 TM    CSLSTCUR+(LSTPSTAT-LSTTABD),TRNSAPPR (SELECTED/APPROVED)         
         BZ    PSTTXD60                                                         
         LA    RF,VENDLIST                                                      
PSTTXD58 CLI   0(RF),0                                                          
         BE    PSTTXD60                                                         
         CLC   TRNKUNT(UNTLDGL),0(RF)                                           
         BE    *+12                                                             
         LA    RF,UNTLDGL(RF)                                                   
         B     PSTTXD58                                                         
*&&UK*&& LA    R0,L'LC@SELED-1                                                  
*&&UK*&& LH    RF,=Y(LC@SELED-TWAD)                                             
*&&US*&& LA    R0,L'LC@APRVD-1                                                  
*&&US*&& LH    RF,=Y(LC@APRVD-TWAD)                                             
         LA    RF,TWAD(RF)                                                      
         GOTO1 PSTSLC,BOPARM,((R0),(RF)),0                                      
*                                                                               
PSTTXD60 TM    CSLSTCUR+(LSTPSTAT-LSTTABD),TRNSBREC (RECONCILED)                
         BZ    PSTTXD70                                                         
         LA    RF,BANKLIST                                                      
PSTTXD62 CLI   0(RF),0                                                          
         BE    PSTTXD70                                                         
         CLC   TRNKUNT(UNTLDGL),0(RF)                                           
         BE    *+12                                                             
         LA    RF,UNTLDGL(RF)                                                   
         B     PSTTXD62                                                         
         LA    R0,L'LC@RCND-1                                                   
         LH    RF,=Y(LC@RCND-TWAD)                                              
         LA    RF,TWAD(RF)                                                      
         GOTO1 PSTSLC,BOPARM,((R0),(RF)),0                                      
*                                                                               
PSTTXD70 GOTO1 DISTIM,BCWORK       DISPLAY TYPE OF TIME STATUS                  
         LR    R0,R1                                                            
         SH    R0,=H'2'                                                         
         LA    R1,BCWORK                                                        
         CR    R0,R1                                                            
         BL    PSTTXD71                                                         
         SR    R0,R1                                                            
         LA    RF,BCWORK                                                        
         GOTO1 PSTSLC,BOPARM,((R0),(RF)),0                                      
*                                                                               
PSTTXD71 OC    BOBYTE2,BOBYTE2                                                  
         BZ    PSTTXD72                                                         
         LA    R0,L'LC@VPB-1                                                    
         LH    RF,=Y(LC@VPB-TWAD)                                               
         LA    RF,TWAD(RF)                                                      
         MVI   BCWORK,C'='                                                      
         MVC   BCWORK+1(1),BOBYTE2                                              
         GOTO1 PSTSLC,BOPARM,((R0),(RF)),(1,BCWORK)                             
*                                                                               
         USING PRTELD,RF                                                        
PSTTXD72 SR    R0,R0                                                            
         LA    RF,TRNRFST                                                       
PSTTX72A CLI   0(RF),0                                                          
         BE    PSTTXD74                                                         
         CLI   0(RF),PRTELQ        PERSONNEL RATE ELEMENT                       
         BE    *+14                                                             
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     PSTTX72A                                                         
*                                                                               
         LH    RE,=Y(LC@TIME-TWAD)                                              
         LA    RE,TWAD(RE)                                                      
         MVC   BCWORK(L'LC@TIME),0(RE)                                          
         LA    R1,BCWORK                                                        
         LA    R1,L'LC@TIME-1(R1)                                               
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVC   1(L'BCEQUAL,R1),BCEQUAL                                          
         TM    PRTSTAT,PRTSBILQ    BILLABLE TIME                                
         BZ    *+12                                                             
         MVI   2(R1),C'B'                                                       
         B     PSTTXD73                                                         
*                                                                               
         TM    PRTSTAT,PRTSNOTQ    NON-BILLABLE TIME                            
         BZ    *+12                                                             
         MVI   2(R1),C'N'                                                       
         B     PSTTXD73                                                         
*                                                                               
         TM    PRTSTAT,PRTSRTEQ    SPECIAL NON-BILLABLE TIME                    
         BZ    PSTTXD74                                                         
         MVI   2(R1),C'R'                                                       
         DROP  RF                                                               
*                                                                               
PSTTXD73 MVC   3(1,R1),BCCOMMA                                                  
         LA    R1,2(R1)                                                         
         LR    R0,R1                                                            
         LA    R1,BCWORK                                                        
         SR    R0,R1                                                            
         LA    RF,BCWORK                                                        
         GOTO1 PSTSLC,BOPARM,((R0),(RF)),0                                      
*                                                                               
PSTTXD74 TM    CUSTAT,CUSDDS       TEST DDS USER                                
         BZ    PSTTXD76                                                         
         MVC   BCWORK,BCSPACES                                                  
         MVC   BCWORK(L'LSTTDA),CSLSTCUR+(LSTTDA-LSTTABD)                       
         XOUT  BCWORK,BCWORK+4,4                                                
         MVC   BCWORK(4),=C'D/A='  DISPLAY DISK ADDRESS                         
         GOTO1 PSTSLC,BOPARM,(12,BCWORK),0                                      
*                                                                               
PSTTXD76 DS    0H                                                               
PSTTXD78 DS    0H                                                               
*                                                                               
PSTTXD80 LA    R1,BOWORK1          ANY STATUS INFORMATION                       
         CR    R2,R1                                                            
         BNH   PSTTXD82                                                         
         BCTR  R2,0                REMOVE LAST SEPARATOR                        
         CLC   0(L'BCCOMMA,R2),BCCOMMA                                          
         MVI   0(R2),C' '                                                       
         BNE   PSTTXD80                                                         
         MVC   PTXSTAT,BOWORK1     MOVE STATUS WORDS TO SCREEN                  
         MVC   PTXSTA2,BOWORK2                                                  
*                                                                               
PSTTXD82 CLI   CSACT,ACTDSP        DISPLAY OR ANALYSE?                          
         BNE   PSTANA                                                           
         EJECT                                                                  
***********************************************************************         
* EXTRACT DATES AND NARRATIVE                                         *         
***********************************************************************         
         SPACE 1                                                                
PSTDSP   LA    RF,PDPAYLST                                                      
         ST    RF,BOADDR1                                                       
         XC    BOBYTE1,BOBYTE1     PAYMENT ITEMS ACCUMULATOR                    
         LA    RF,BOWORK1                                                       
         ST    RF,PDGTXSUB         A(GETTXT SUBSTITUTION TABLE)                 
*                                                                               
         LA    R2,BCBATCUR         R2=A(BATCH LSTTAB ENTRY)                     
         USING LSTTABD,R2                                                       
         MVC   PDDRAFT,LSTBADDT    DRAFTED                                      
         MVC   PDEFF,LSTBEFDT      LIVE                                         
*                                                                               
         TM    TRNRSTA2,TRNSUSED   TEST USED TRANSACTION                        
         BZ    PDSP02                                                           
         CLI   BOFLAG1,PDDSSUP     SUPPRESS VENDOR DEBIT MOA                    
         BNE   *+12                                                             
         TM    CSLSTCUR+(LSTPSTAT-LSTTABD),TRNSDR                               
         BO    PDSP02                                                           
         MVC   PDUSEDMA,TRNRSUSE   SET USED MOA                                 
         DROP  R2                                                               
*                                                                               
PDSP02   LA    R2,TRNRFST          R2=A(FIRST TRANSACTION ELEMENT)              
PDSP04   CLI   0(R2),0                                                          
         BE    PDSP44                                                           
         CLI   0(R2),TRNELQ        TRANSACTION ELEMENT                          
         BE    PDSP08                                                           
         CLI   0(R2),ADRELQ        ADDRESS ELEMENT (SUNDRY CREDITOR)            
         BE    PDSP10                                                           
         CLI   0(R2),FFNELQ        FREE FORM/ORDER NUMBER ELEMENT               
         BE    PDSP14                                                           
         CLI   0(R2),TRSELQ        TRANSACTION STATUS ELEMENT                   
         BE    PDSP16                                                           
         CLI   0(R2),DUEELQ        DUE DATE ELEMENT                             
         BE    PDSP18                                                           
         CLI   0(R2),MPYELQ        MANUAL PAYMENT ELEMENT                       
         BE    PDSP20                                                           
         CLI   0(R2),OAMELQ        ORDER AMOUNT ELEMENT                         
         BE    PDSP22                                                           
         CLI   0(R2),SORELQ        SOURCE ACCOUNT ELEMENT                       
         BE    PDSP24                                                           
         CLI   0(R2),RALELQ        RECEIVABLE ALLOCATION ELEMENT                
         BE    PDSP28                                                           
         CLI   0(R2),AFCELQ        FOREIGN CURRENCY ELEMENT                     
         BE    PDSP40                                                           
PDSP06   XR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     PDSP04                                                           
*                                                                               
         USING TRNELD,R2                                                        
PDSP08   XC    BOELEM,BOELEM                                                    
         CLI   TRNLN,TRNLN1Q       ANY NARRATIVE?                               
         BE    PDSP06                                                           
         LA    RF,PDPRDNLN         FIXED LENGTH FOR WORKCODE 99                 
         CLC   TRNKUNT(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         BNE   *+8                                                              
         CLI   TRNLN,121           TEST OLD STYLE BILLING NARRATIVE             
         BNE   *+14                                                             
         CLC   TRNANAL,=C'99'                                                   
         BE    *+14                                                             
         XR    RF,RF                                                            
         IC    RF,TRNLN                                                         
         SH    RF,=Y(TRNLN1Q)                                                   
         STC   RF,PDNARLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     PDSP06                                                           
         MVC   BOELEM(0),TRNNARR                                                
*                                                                               
         USING ADRELD,R2                                                        
PDSP10   CLI   PDSUNDRY,C'Y'                                                    
         BNE   PDSP06                                                           
         XR    R0,R0                                                            
         ICM   R0,1,ADRNUM                                                      
         BZ    PDSP06                                                           
         STC   R0,PDSCDADN                                                      
         LA    RE,ADRADD1                                                       
         LA    R1,PDSCDAD1                                                      
PDSP12   MVC   0(L'PDSCDAD1,R1),0(RE)                                           
         LA    R1,L'PDSCDAD1(R1)                                                
         LA    RE,L'ADRADD1(RE)                                                 
         BCT   R0,PDSP12                                                        
         B     PDSP06                                                           
*                                                                               
         USING FFNELD,R2                                                        
PDSP14   CLC   PDUNLD(L'PDUNLD),BCCPYEL+(CPYPROD-CPYELD)                        
         BNE   PDSP06                                                           
         LA    R4,PDORDDET                                                      
         USING ORDDETD,R4                                                       
         MVC   PDORDDET(ORDDETQ),BCSPACES                                       
         MVC   ORDDNUM,FFNONUM                                                  
         L     RE,PDGTXSUB         BUILD GETTXT TEXT TABLE                      
         LA    RF,L'ORDDNUM+1                                                   
         STC   RF,0(RE)                                                         
         EX    RF,*+4                                                           
         MVC   1(0,RE),ORDDNUM                                                  
         AR    RE,RF                                                            
         ST    RE,PDGTXSUB                                                      
         B     PDSP06                                                           
         DROP  R4                                                               
*                                                                               
         USING TRSELD,R2                                                        
PDSP16   MVC   PDREVD,TRSREVD      REVERSED                                     
         MVC   PDUPDAT,TRSUPDT     UPDATED                                      
         MVC   PDUSED,TRSUDAT      USED                                         
         MVC   PDPEEL,TRSPDAT      PEELED                                       
         MVC   PDREVDMA,TRSRMOS    REVERSING MOA                                
         MVC   PDPOSTMA,TRSPMOS    POSTING MOA                                  
         TM    TRNRSTAT,TRNSDRFT   NO LIVE DATE ON DRAFT TRANSACTIONS           
         BO    PDSP06                                                           
         XC    PDEFF,PDEFF         EFFECTIVE ON                                 
         MVC   PDLIVE,TRSDATE      LIVE ON                                      
         B     PDSP06                                                           
*                                                                               
         USING DUEELD,R2                                                        
PDSP18   MVC   PDDUE,DUEDATE       DUE ON                                       
         B     PDSP06                                                           
*                                                                               
         USING MPYELD,R2                                                        
PDSP20   CP    MPYAMNT,BCPZERO                                                  
         BE    PDSP06                                                           
         XR    R1,R1                                                            
         IC    R1,BOBYTE1                                                       
         LA    R1,1(R1)                                                         
         STC   R1,BOBYTE1                                                       
         CLI   BOBYTE1,PDMPAYM                                                  
         BH    PDSP06                                                           
         L     R4,BOADDR1                                                       
         USING PYLISTD,R4                                                       
         MVC   PYLNUM,MPYNO                                                     
         GOTO1 VDATCON,BOPARM,('PDDC2',MPYDTE),('PDDLG',PYLDATE)                
*&&US*&& CURED MPYAMNT,(L'PYLAMT,PYLAMT),2,MINUS=YES                            
*&&UK*&& CURED MPYAMNT,(L'PYLAMT,PYLAMT),2,FLOAT=-                              
         LA    R4,PYLISTQ(R4)                                                   
         ST    R4,BOADDR1                                                       
         B     PDSP06                                                           
         DROP  R4                                                               
*                                                                               
         USING OAMELD,R2                                                        
PDSP22   MVC   PDOLACT,OAMLAST     ORDER LAST ACTIVE                            
         B     PDSP06                                                           
*                                                                               
         USING SORELD,R2                                                        
PDSP24   CLI   SORSYS,SORSACC      SOURCE - ACCOUNT SYSTEM                      
         BNE   PDSP26                                                           
         LA    RF,PDSORDET                                                      
         MVC   0(L'SORAUNT+L'SORALDG,RF),=C'SO'  OTHER                          
         CLC   =C'SJ',SORAUNT      PRODUCTION                                   
         BE    *+14                                                             
         CLC   =C'SE',SORAUNT      EXPENSE                                      
         BNE   *+10                                                             
         MVC   0(L'SORAUNT+L'SORALDG,RF),SORAUNT                                
         LA    RF,L'SORAUNT+L'SORALDG(RF)                                       
         MVC   0(L'BCEQUAL,RF),BCEQUAL                                          
         MVC   L'BCEQUAL(L'SORAACT,RF),SORAACT                                  
         B     PDSP06                                                           
*                                                                               
PDSP26   CLI   SORSYS,SORSMED      SOURCE - MEDIA SYSTEM                        
         BNE   PDSP06                                                           
         MVC   BOWORK2,BCSPACES                                                 
         MVC   BOWORK2(L'SORMCLI),SORMCLI                                       
         MVC   BOWORK2+L'SORMCLI+1(L'SORMPRO),SORMPRO                           
         MVC   BOWORK2+L'SORMCLI+L'SORMPRO+2(L'SORMCAM),SORMCAM                 
         LA    R4,L'SORMCLI+L'SORMPRO+L'SORMCAM+2                               
         GOTO1 VSQUASH,BOPARM,BOWORK2,(C'/',(R4))                               
         LA    RF,PDSORDET                                                      
         MVC   0(L'SORAUNT+L'SORALDG,RF),=C'SM'                                 
         LA    RF,L'SORAUNT+L'SORALDG(RF)                                       
         MVC   0(L'SORMMED,RF),SORMMED                                          
         MVC   1(L'BCEQUAL,RF),BCEQUAL                                          
         L     RE,4(R1)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     PDSP06                                                           
         MVC   L'BCEQUAL+1(0,RF),BOWORK2                                        
*                                                                               
         USING RALELD,R2                                                        
PDSP28   LA    R4,PDRECDET                                                      
         USING RECDETD,R4                                                       
         MVC   PDRECTYP,RALTYPE                                                 
         CLI   RALTYPE,RALTALC     REGULAR ALLOCATION                           
         BE    PDSP30                                                           
         CLI   RALTYPE,RALTOFS     OFFSET/CONTRA'D                              
         BE    PDSP32                                                           
         CLI   RALTYPE,RALTWOF     WRITTEN-OFF                                  
         BE    PDSP34                                                           
         CLI   RALTYPE,RALTTTO     TRANSFERRED TO                               
         BE    PDSP36                                                           
         CLI   RALTYPE,RALTTFR     TRANSFERRED FROM                             
         BE    PDSP36                                                           
         B     PDSP06                                                           
*                                                                               
PDSP30   GOTO1 VDATCON,BOPARM,('PDDP3',RALADAT),('PDDLG',RECDAT1)               
         GOTO1 (RF),(R1),('PDDP3',RALADEP),('PDDLG',RECDAT2)                    
         MVC   RECREF,RALAREF                                                   
         B     PDSP38                                                           
*                                                                               
PDSP32   GOTO1 VDATCON,BOPARM,('PDDP3',RALODAT),('PDDLG',RECDAT1)               
         B     PDSP38                                                           
*                                                                               
PDSP34   GOTO1 VDATCON,BOPARM,('PDDP3',RALWDAT),('PDDLG',RECDAT1)               
*&&UK*&& GOTO1 (RF),(R1),('PDDP3',RALWDEP),('PDDLG',RECDAT2)                    
         MVC   RECACC(L'RALWULA),RALWULA                                        
         MVC   RECREF(L'RALWREF),RALWREF                                        
         B     PDSP38                                                           
*                                                                               
PDSP36   GOTO1 VDATCON,BOPARM,('PDDP3',RALTDAT),('PDDLG',RECDAT1)               
         MVC   BOFULL1,RALTMOS                                                  
         MVI   BOFULL1+2,1                                                      
         GOTO1 (RF),(R1),('PDDP2',BOFULL1),('PDDSH',RECMOA)                     
         MVC   RECACC(L'RALTULA),RALTULA  TRANSFER ACCOUNT                      
*                                                                               
PDSP38   L     RE,PDGTXSUB         BUILD GETTXT TEXT TABLE                      
         LA    RF,L'RECDAT1+1                                                   
         STC   RF,0(RE)                                                         
         EX    RF,*+4                                                           
         MVC   1(0,RE),RECDAT1                                                  
         AR    RE,RF                                                            
         STC   RF,0(RE)                                                         
         EX    RF,*+4                                                           
         MVC   1(0,RE),RECDAT2                                                  
         AR    RE,RF                                                            
         LA    RF,L'RECMOA+1                                                    
         STC   RF,0(RE)                                                         
         EX    RF,*+4                                                           
         MVC   1(0,RE),RECMOA                                                   
         AR    RE,RF                                                            
         LA    RF,L'RECREF+1                                                    
         STC   RF,0(RE)                                                         
         EX    RF,*+4                                                           
         MVC   1(0,RE),RECREF                                                   
         AR    RE,RF                                                            
         LA    RF,L'RECACC+1                                                    
         STC   RF,0(RE)                                                         
         EX    RF,*+4                                                           
         MVC   1(0,RE),RECACC                                                   
         AR    RE,RF                                                            
         MVI   0(RE),0                                                          
         LA    RF,BOWORK1+L'BOWORK1-1                                           
         CR    RE,RF                                                            
         BNH   PDSP06                                                           
         DC    H'0'                TABLE TOO LARGE                              
         DROP  R2,R4                                                            
*                                                                               
         USING AFCELD,R2                                                        
PDSP40   MVC   PDCURC,AFCCURR      CURRENCY CODE                                
*                                  READ CURRENCY TABLE ENTRY                    
         GOTO1 VBLDCUR,BOPARM,PDCURC,(X'00',BOWORK2),ACOM                       
         BNE   PDSP06              NOT FOUND                                    
*                                  EDIT CURRENCY AMOUNT                         
         CURED (P6,AFCAMNT),(L'PDCAMT,PDCAMT),BOWORK2,MINUS=YES,       >        
               ALIGN=LEFT,CURSYMB=YES                                           
*                                  EDIT EXCHANGE RATE                           
         XC    BODUB1,BODUB1                                                    
         MVO   BODUB1,AFCXRATE                                                  
         OI    BODUB1+L'BODUB1-1,X'0C'  CONVERT TO PACKED                       
         CURED (P8,BODUB1),(L'PDXRAT,PDXRAT),5,ALIGN=LEFT                       
*                                                                               
         LA    RF,PDXRAT+L'PDXRAT-1                                             
PDSP42   CLI   0(RF),C','          DROP TRAILING ZEROS                          
         BE    *+8                                                              
         CLI   0(RF),C'.'                                                       
         BNE   *+12                                                             
         MVI   1(RF),C'0'                                                       
         B     PDSP06                                                           
         CLI   0(RF),C'0'                                                       
         BH    PDSP06                                                           
         MVI   0(RF),C' '                                                       
         BCT   RF,PDSP42                                                        
         B     PDSP06                                                           
*                                                                               
PDSP44   CLC   PDUNLD(L'PDUNLD),BCCPYEL+(CPYPROD-CPYELD)                        
         BNE   PDSP50                                                           
         LA    R4,PDORDDET                                                      
         USING ORDDETD,R4                                                       
*                                                                               
         LA    R2,IOKEY            READ ORDER RECORD                            
         USING ORDRECD,R2                                                       
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUABIN                                                   
         MVC   ORDKORD,ORDDNUM                                                  
         GOTO1 AIO,IORD+IOACCMST+IO2                                            
         BE    *+14                                                             
         MVC   ORDDNUM,BCSPACES                                                 
         B     PDSP50              CANNOT FIND ANY ORDER DETAILS                
*                                                                               
         L     R2,AIO2                                                          
         LA    R2,ORDRFST                                                       
PDSP46   CLI   0(R2),0                                                          
         BE    PDSP50                                                           
         CLI   0(R2),ORDELQ        PRODUCTION ORDER ELEMENT                     
         BE    PDSP48                                                           
         XR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     PDSP46                                                           
*                                                                               
         USING ORDELD,R2                                                        
PDSP48   GOTO1 VDATCON,BOPARM,('PDDP3',ORDDATE),('PDDLG',ORDDDAT)               
         MVC   ORDDACC,ORDSUPU                                                  
         MVC   ORDDAUT,ORDAUTH                                                  
         L     RE,PDGTXSUB         CONTINUE BUILDING GETTXT TEXT TABLE          
         LA    RF,L'ORDDDAT+1                                                   
         STC   RF,0(RE)                                                         
         EX    RF,*+4                                                           
         MVC   1(0,RE),ORDDDAT                                                  
         AR    RE,RF                                                            
         LA    RF,L'ORDDACC+1                                                   
         STC   RF,0(RE)                                                         
         EX    RF,*+4                                                           
         MVC   1(0,RE),ORDDACC                                                  
         AR    RE,RF                                                            
         MVI   0(RE),0                                                          
         LA    RE,1(RE)                                                         
         ST    RE,PDGTXSEX         A(START OF TABLE EXTENSION)                  
         LA    RF,L'ORDDAUT+1                                                   
         STC   RF,0(RE)                                                         
         EX    RF,*+4                                                           
         MVC   1(0,RE),ORDDAUT                                                  
         AR    RE,RF                                                            
         MVI   0(RE),0                                                          
         L     RF,PDGTXSEX                                                      
         SR    RE,RF                                                            
         STC   RE,PDLENSEX                                                      
         LA    RF,BOWORK1+L'BOWORK1-1                                           
         CR    RE,RF                                                            
         BNH   PDSP50                                                           
         DC    H'0'                   TABLE TOO LARGE                           
         DROP  R2,R4                                                            
         SPACE 1                                                                
PYLISTD  DSECT                     ** PAYMENTS TABLE **                         
PYLNUM   DS    CL6                 NUMBER                                       
PYLDATE  DS    CL8                 DATE                                         
PYLAMT   DS    XL12                AMOUNT                                       
PYLISTQ  EQU   *-PYLISTD                                                        
         SPACE 1                                                                
PYLWSEP  EQU   4                   SEPARATOR WITHIN PAYMENTS                    
PYLBSEP  EQU   10                  SEPARATOR BETWEEN PAYMENTS                   
         SPACE 1                                                                
RECDETD  DSECT                     ** RECEIVABLES DETAILS **                    
RECDAT1  DS    CL8                 DATE 1                                       
RECDAT2  DS    CL8                 DATE 2                                       
RECMOA   DS    CL6                 MOA                                          
RECREF   DS    CL6                 REFERENCE                                    
RECACC   DS    CL14                ACCOUNT                                      
RECDETQ  EQU   *-RECDETD                                                        
         SPACE 1                                                                
ORDDETD  DSECT                     ** ORDER DETAILS **                          
ORDDNUM  DS    CL6                 NUMBER                                       
ORDDDAT  DS    CL8                 DATE                                         
ORDDACC  DS    CL14                ACCOUNT                                      
ORDDAUT  DS    CL15                AUTHORISED BY                                
ORDDETQ  EQU   *-ORDDETD                                                        
BAT62    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY DATES AND NARRATIVE                                         *         
***********************************************************************         
         SPACE 1                                                                
PDSP50   LA    R2,PTXLIN1          R2=A(FIRST DATE LINE)                        
         ST    R2,BOADDR2                                                       
         LA    R0,L'PTXLIN1        LINE LENGTH                                  
         XC    BOBYTE1,BOBYTE1     LINE COUNTER                                 
*                                                                               
         LA    R3,PDDTAB           R3=A(DATE DISPLAY TABLE)                     
         USING PDDTABD,R3                                                       
PDSP52   CLC   PDDLST,PDDTABX      END OF TABLE                                 
         BNE   PDSP54                                                           
         XR    R1,R1                                                            
         IC    R1,BOBYTE1                                                       
         LA    R1,1(R1)            INCREMENT LINE TOTAL                         
         STC   R1,BOBYTE1                                                       
         B     PDSP64                                                           
PDSP54   XR    R4,R4                                                            
         ICM   R4,3,PDDLST                                                      
         LA    R4,PDWORKD(R4)                                                   
         XC    BOFULL1,BOFULL1                                                  
         XR    RF,RF                                                            
         IC    RF,PDDLDT                                                        
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   BOFULL1(0),0(R4)                                                 
         OC    BOFULL1,BOFULL1                                                  
         BZ    PDSP56                                                           
         CLI   PDDSUBS,0           SUBSTATUS                                    
         BE    PDSP58                                                           
         CLC   PDDSUBS,BOFLAG1                                                  
         BE    PDSP58                                                           
PDSP56   LA    R3,PDDTABL(R3)                                                   
         B     PDSP52                                                           
*                                                                               
PDSP58   XR    RF,RF                                                            
         ICM   RF,3,PDDPREF                                                     
         LA    RF,TWAD(RF)         PREFIX                                       
         XR    RE,RE                                                            
         IC    RE,PDDLPREF         L'PREFIX                                     
         CLI   PDDLPREF,PDPRMX                                                  
         BNH   *+8                                                              
         LA    RE,PDPRMX                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)       MOVE PREFIX TO DISPLAY LINE                  
         LR    RF,R2                                                            
         AR    RF,RE                                                            
         CLI   0(RF),C' '                                                       
         BH    PDSP60                                                           
         CR    RF,R2                                                            
         BE    *+8                                                              
         BCT   RF,*-14                                                          
         DC    H'0'                                                             
*                                                                               
PDSP60   CLI   PDDCIN,PDDC2        PACKED OR COMPRESSED?                        
         BE    *+16                                                             
         CLI   PDDLDT,PDDLD3       2 OR 3 BYTE PACKED DATE?                     
         BE    *+8                                                              
         MVI   BOFULL1+PDDLD2,1    CONVERT 2 TO 3 BYTE                          
         LA    R4,BOFULL1                                                       
         GOTO1 VDATCON,BOPARM,(PDDCIN,(R4)),(PDDCOUT,2(RF))                     
*                                                                               
         LA    RE,PDSTDC+PDSTDS                                                 
         SR    R0,RE               LINE LENGTH REMAINING                        
         AR    R2,RE               SET FOR NEXT DATE                            
         LA    RF,PDSTDC                                                        
         CR    R0,RF               CHECK NEXT DATE FITS                         
         BNL   PDSP62                                                           
         XR    R1,R1                                                            
         IC    R1,BOBYTE1                                                       
         LA    R1,1(R1)                                                         
         STC   R1,BOBYTE1                                                       
         LA    RF,PDDLINM          MAXIMUM NUMBER DATE LINES                    
         CR    R1,RF                                                            
         BNL   PDSP64              DATE LINES FULL                              
         L     R2,BOADDR2                                                       
         LA    R2,L'PTXLIN1H+L'PTXLIN1(R2)  NEXT DATE LINE                      
         ST    R2,BOADDR2                                                       
         LA    R0,L'PTXLIN1        RESET LINE LENGTH                            
PDSP62   LA    R3,PDDTABL(R3)      NEXT DATE LIST ENTRY                         
         B     PDSP52                                                           
         DROP  R3                                                               
*                                                                               
PDSP64   L     RF,BOADDR2                                                       
         CR    R2,RF               WAS LAST LINE USED?                          
         BE    *+10                                                             
         LA    RF,L'PTXLIN1H+L'PTXLIN1(RF)                                      
         LR    R2,RF                                                            
         LA    R2,L'PTXLIN1+L'PTXLIN1H(R2)                                      
         ST    R2,BOADDR2          R2/BOADDR2=A(HEADING LINE)                   
*                                                                               
         OC    PDRECTYP,PDRECTYP   RECEIVABLES DATA                             
         BZ    PDSP66                                                           
         LA    R3,AS$RECDT                                                      
         BAS   RE,PSTOTHHD         DISPLAY RECEIVABLES HEADING                  
         SH    R2,=Y(L'PTXLIN1H)   R2=A(HEADER FOR FIRST DATA LINE)             
*                                                                               
         CLI   PDRECTYP,RALTALC    REGULAR ALLOCATION                           
         BNE   *+16                                                             
         LA    R3,AS$REGAL                                                      
         BAS   RE,PSTOTHDT         DISPLAY DETAILS                              
         B     PDSP76                                                           
         SPACE 1                                                                
         CLI   PDRECTYP,RALTOFS    OFFSET                                       
         BNE   *+16                                                             
         LA    R3,AS$OFFST                                                      
         BAS   RE,PSTOTHDT                                                      
         B     PDSP76                                                           
         SPACE 1                                                                
         CLI   PDRECTYP,RALTWOF    WRITE-OFF                                    
         BNE   *+16                                                             
         LA    R3,AS$WROFF                                                      
         BAS   RE,PSTOTHDT                                                      
         B     PDSP76                                                           
         SPACE 1                                                                
         CLI   PDRECTYP,RALTTTO    TRANSFER TO                                  
         BNE   *+16                                                             
         LA    R3,AS$TRNTO                                                      
         BAS   RE,PSTOTHDT                                                      
         B     PDSP76                                                           
         SPACE 1                                                                
         CLI   PDRECTYP,RALTTFR    TRANSFER FROM                                
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,AS$TRNFM                                                      
         BAS   RE,PSTOTHDT                                                      
         B     PDSP76                                                           
*                                                                               
PDSP66   LA    R4,PDPAYLST                                                      
         USING PYLISTD,R4                                                       
         OC    0(PYLISTQ,R4),0(R4)                                              
         BZ    PDSP74                                                           
         LA    R3,AS$DATDT                                                      
         BAS   RE,PSTOTHHD         SET UP PAYMENTS HEADING                      
         XR    R1,R1                                                            
         IC    R1,BOBYTE1                                                       
         LA    R1,1(R1)            INCREMENT LINE TOTAL                         
         STC   R1,BOBYTE1                                                       
         LA    R5,L'PTXLIN1        LINE LENGTH                                  
*                                                                               
PDSP68   XC    BOWORK1,BOWORK1     BUILD PAYMENT DETAILS HERE                   
         LA    R3,BOWORK1                                                       
         MVC   0(L'PYLNUM,R3),PYLNUM                                            
         LA    R3,L'PYLNUM+PYLWSEP(R3)                                          
         MVC   0(L'PYLDATE,R3),PYLDATE                                          
         LA    R3,L'PYLDATE+PYLWSEP(R3)                                         
         MVC   0(L'PYLAMT,R3),PYLAMT                                            
         LA    R3,L'PYLAMT(R3)                                                  
         LA    R0,BOWORK1                                                       
         SR    R3,R0               LENGTH OF PAYMENT DETAILS                    
*                                                                               
PDSP70   SR    R5,R3               CHECK DETAILS FIT ON LINE                    
         BNM   PDSP72                                                           
         XR    R1,R1               CHECK IF ANY MORE LINES AVAILABLE            
         IC    R1,BOBYTE1                                                       
         LA    R1,1(R1)                                                         
         LA    RE,PDDLINM+PDOLINM                                               
         CR    R1,RE                                                            
         BNL   PDSP97                                                           
         STC   R1,BOBYTE1                                                       
         L     R2,BOADDR2                                                       
         LA    R2,L'PTXLIN1H+L'PTXLIN1(R2)                                      
         ST    R2,BOADDR2                                                       
         LA    R5,L'PTXLIN1        RESET LINE LENGTH                            
         B     PDSP70                                                           
PDSP72   BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   0(0,R2),BOWORK1                                                  
*                                                                               
         LA    R4,PYLISTQ(R4)      NEXT ITEM ON PAYMENT LIST                    
         OC    0(PYLISTQ,R4),0(R4) ANYTHING THERE                               
         BZ    PDSP76                                                           
         AR    R2,R3                                                            
         LA    R2,PYLBSEP(R2)      SET FOR NEXT PAYMENT                         
         SH    R5,=Y(PYLBSEP)      LINE LENGTH LEFT AFTER SEPARATOR             
         B     PDSP68                                                           
         DROP  R4                                                               
*                                                                               
PDSP74   CLC   PDUNLD(L'PDUNLD),BCCPYEL+(CPYPROD-CPYELD)                        
         BNE   PDSP78                                                           
         LA    R4,PDORDDET                                                      
         USING ORDDETD,R4                                                       
         CLC   ORDDNUM,BCSPACES                                                 
         BE    PDSP78                                                           
         LA    R3,AS$ORDDT                                                      
         BAS   RE,PSTOTHHD         DISPLAY ORDERS HEADING                       
         SH    R2,=Y(L'PTXLIN1H)   R2=A(HEADER FOR FIRST DATA LINE)             
         LA    R3,AS$ORD#1                                                      
         BAS   RE,PSTOTHDT         DISPLAY ORDER DETAILS                        
         LA    R2,L'PTXLIN1H+L'PTXLIN1H+L'PTXLIN1(R2)                           
         ST    R2,BOADDR2                                                       
         SH    R2,=Y(L'PTXLIN1H)                                                
         L     RF,PDGTXSEX         A(GETTXT SUB TABLE EXTENSION)                
         XR    RE,RE                                                            
         IC    RE,PDLENSEX                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   BOWORK1(0),0(RF)                                                 
         LA    R3,AS$ORD#2                                                      
         BAS   RE,PSTOTHDT                                                      
         DROP  R4                                                               
*                                                                               
PDSP76   XR    R1,R1               CHECK IF ANY MORE LINES AVAILABLE            
         IC    R1,BOBYTE1                                                       
         LA    R1,1(R1)                                                         
         LA    RE,PDDLINM+PDOLINM                                               
         CR    R1,RE                                                            
         BNL   PDSP97                                                           
         STC   R1,BOBYTE1                                                       
         L     R2,BOADDR2          SET R2=A(NEXT LINE)                          
         LA    R2,L'PTXLIN1+L'PTXLIN1H(R2)                                      
*                                                                               
PDSP78   OC    PDCURC,PDCURC       FOREIGN CURRENCY DETAILS                     
         BZ    PDSP80                                                           
         LA    R3,AS$CURDT                                                      
         BAS   RE,PSTOTHHD                                                      
         SH    R2,=Y(L'PTXLIN1H)   R2=A(FLDHDR)                                 
         XC    BOWORK1,BOWORK1                                                  
         LA    R3,BOWORK1                                                       
         LA    RF,L'PDCAMT+1       BUILD SUBSTITUTION TEXT FOR GETTXT           
         STC   RF,0(R3)                                                         
         MVC   1(L'PDCAMT,R3),PDCAMT                                            
         LA    R3,0(RF,R3)                                                      
         LA    RF,L'PDXRAT+1                                                    
         STC   RF,0(R3)                                                         
         MVC   1(L'PDXRAT,R3),PDXRAT                                            
         LA    R3,AS$CURXA                                                      
         BAS   RE,PSTOTHDT                                                      
*                                                                               
         XR    R1,R1               CHECK IF ANY MORE LINES AVAILABLE            
         IC    R1,BOBYTE1                                                       
         LA    RE,PDDLINM+PDOLINM                                               
         CR    R1,RE                                                            
         BNL   PDSP97                                                           
         L     R2,BOADDR2          SET R2=A(NEXT LINE)                          
         LA    R2,L'PTXLIN1+L'PTXLIN1H(R2)                                      
*                                                                               
PDSP80   OC    PDSCDAD1,PDSCDAD1   SUNDRY CREDITOR ADDRESS                      
         BZ    PDSP96                                                           
         LA    R3,AS$SCDAD                                                      
         BAS   RE,PSTOTHHD                                                      
         XR    R1,R1                                                            
         ICM   R1,1,PDSCDADN                                                    
         BZ    PDSP95                                                           
         LA    RE,PDSCDAD1                                                      
         LA    R3,BOWORK1                                                       
         MVC   BOWORK1,BCSPACES                                                 
         MVC   BOWORK2,BCSPACES                                                 
PDSP82   MVC   0(L'PDSCDAD1,R3),0(RE)                                           
         LR    R0,R3                                                            
         LA    R3,L'PDSCDAD1-1(R3)                                              
         B     *+10                                                             
PDSP84   CR    R0,R3                                                            
         BNL   PDSP86                                                           
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,PDSP84                                                        
         MVI   1(R3),C','                                                       
         LA    R3,2(R3)                                                         
         LA    RE,L'PDSCDAD1(RE)                                                
PDSP86   BCT   R1,PDSP82                                                        
*                                                                               
         LA    RE,BOWORK1          START OF ADDRESS DETAILS                     
         LA    RF,L'PTXLIN1        RF=L'SCREEN LINE                             
         LA    R0,PDCLINM          MAXIMUM NO. OF LINES AVAILABLE               
*                                                                               
PDSP88   LA    R1,0(RF,RE)                                                      
PDSP90   CLI   0(R1),C','          FIND LAST REAL BREAK IN LINE                 
         BE    PDSP92                                                           
         CR    R1,RE                                                            
         BNH   *+8                                                              
         BCT   R1,PDSP90                                                        
*                                                                               
         LR    R1,RF               MOVE WHOLE LINE IF NO BREAK FOUND            
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R2),0(RE)                                                    
         LA    RE,1(R1,RE)                                                      
         B     PDSP94                                                           
*                                                                               
PDSP92   SR    R1,RE               MOVE LINE AS FAR AS REAL BREAK               
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R2),0(RE)                                                    
         LA    RE,2(R1,RE)                                                      
*                                                                               
PDSP94   CR    RE,R3               END OF ADDRESS DETAILS                       
         BNL   PDSP95                                                           
         LA    R2,L'PTXLIN1H+L'PTXLIN1(R2)                                      
         BCT   R0,PDSP88                                                        
*                                                                               
PDSP95   XR    R1,R1               CHECK IF ANY MORE LINES AVAILABLE            
         IC    R1,BOBYTE1                                                       
         LA    RE,PDDLINM+PDOLINM                                               
         CR    R1,RE                                                            
         BNL   PDSP97                                                           
         L     R2,BOADDR2          SET R2=A(NEXT LINE)                          
         LA    R2,L'PTXLIN1+L'PTXLIN1H(R2)                                      
*                                                                               
PDSP96   OC    PDSORDET,PDSORDET   SOURCE A/C (OTHER DETAILS)                   
         BZ    PDSP97                                                           
         LA    R3,AS$OTHDT                                                      
         BAS   RE,PSTOTHHD                                                      
         MVC   0(L'PDSORDET,R2),PDSORDET                                        
*                                                                               
PDSP97   LA    R2,PTXLIN1H         SKIP OVER DATE & OTHER LINES                 
         LA    RF,PDDLINM+PDOLINM+1                                             
         LA    R2,L'PTXLIN1H+L'PTXLIN1(R2)                                      
         BCT   RF,*-4                                                           
         NI    FVATRB-FVIHDR(R2),FVFOK-FVAHIGH NON-INTENSIFIED FIELD            
         LA    R2,L'PTXLIN1H(R2)                                                
         ST    R2,BOADDR2                                                       
         MVI   0(R2),C'-'          SET UP NARRATIVE HEADING                     
         MVC   1(L'PTXLIN1-1,R2),0(R2)                                          
         XR    RE,RE                                                            
         LA    RF,L'PTXLIN1-L'LC@NRTV                                           
         LA    R1,2                                                             
         DR    RE,R1                                                            
         AR    R2,RF                                                            
         LH    R1,=Y(LC@NRTV-TWAD)                                              
         LA    R1,TWAD(R1)                                                      
         MVC   0(L'LC@NRTV,R2),0(R1)                                            
*                                                                               
         CLI   PDNARLEN,0          CHECK NARRATIVE EXISTS                       
         BE    PDSPX                                                            
         LA    R0,PDNLINM          MAXIMUM NUMBER OF LINES                      
         GOTO1 VCHOPPER,BOPARM,(PDNARLEN,BOELEM),(L'PTXLIN1,PDNARWRK), X        
               (R0)                                                             
         ICM   R1,15,8(R1)         NUMBER OF LINES                              
         BZ    PDSPX                                                            
         L     R2,BOADDR2                                                       
         LA    RF,PDNARWRK                                                      
PDSP98   LA    R2,L'PTXLIN1H+L'PTXLIN1(R2)                                      
         MVC   0(L'PTXLIN1,R2),0(RF)                                            
         LA    RF,L'PTXLIN1(RF)                                                 
         BCT   R1,PDSP98                                                        
PDSPX    B     PSTTRANS            TRANSMIT ENTIRE SCREEN                       
         SPACE 1                                                                
PDSTDC   EQU   24                  STANDARD COLUMN WIDTH                        
PDSTDS   EQU   3                   STANDARD SEPARATOR WIDTH                     
PDPRMX   EQU   15                  MAXIMUM LENGTH OF DATE PREFIX                
         EJECT                                                                  
***********************************************************************         
* EXTRACT ATTRIBUTE ACCOUNTS AND SUBSIDIARY CASH INFO                 *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R3                                                       
PSTANA   LA    R2,TRNRFST          READ TRANSACTION RECORD ELEMENTS             
         LA    R1,PDSUBLST                                                      
         ST    R1,BOADDR2                                                       
PSTANA02 CLI   0(R2),0                                                          
         BE    PSTANA12                                                         
         CLI   0(R2),SCIELQ        SUB CASH INFO ELEMENT                        
         BE    PSTANA06                                                         
         CLI   0(R2),PRTELQ        PERSONNEL RATE ELEMENT                       
         BE    PSTANA7A                                                         
         CLI   0(R2),APEELQ        ANALYSIS POINTER ELEMENT                     
         BE    PSTANA08                                                         
PSTANA04 XR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     PSTANA02                                                         
*                                                                               
         USING SCIELD,R2                                                        
PSTANA06 L     R1,BOADDR2                                                       
         USING SUBLSTD,R1                                                       
*&&UK                                                                           
         CLI   SCITYPE,SCITMILE                                                 
         BNE   PSTANA07                                                         
         CLC   PDUNLD,=C'SI'                                                    
         BE    PSTANA07                                                         
         SRP   SCIAMNT,2,0                                                      
*&&                                                                             
PSTANA07 MVC   SUBAMNT,SCIAMNT                                                  
         MVC   SUBTYPE,SCITYPE                                                  
         LA    R1,SUBLSTQ(R1)                                                   
         ST    R1,BOADDR2                                                       
         B     PSTANA04                                                         
*                                                                               
         USING PRTELD,R2                                                        
PSTANA7A CLC   PDUNLD,=C'SJ'       FOR SJ POSTINGS ONLY                         
         BNE   PSTANA04                                                         
         L     R1,BOADDR2                                                       
         ZAP   SUBAMNT,PRTRATE                                                  
         MVI   SUBTYPE,SCITPRAT    RATE   'R'                                   
         LA    R1,SUBLSTQ(R1)                                                   
         ZAP   SUBAMNT,PRTHOUR                                                  
         MVI   SUBTYPE,SCITHOUR    HOURS  'H'                                   
         LA    R1,SUBLSTQ(R1)                                                   
         ST    R1,BOADDR2                                                       
         B     PSTANA04                                                         
*                                                                               
         USING APEELD,R2                                                        
         USING ATTLSTD,R5                                                       
PSTANA08 ST    R2,BOADDR1                                                       
         LA    R5,PDATTLST                                                      
         CLI   APELN,APELN1Q       CHECK SUB-ELMTS EXIST                        
         BE    PSTANA04                                                         
         XR    R4,R4                                                            
         IC    R4,APENUM           NUMBER OF SUB-ELEMENTS                       
         LA    R2,APENTRY                                                       
         USING APENTRY,R2                                                       
PSTANA10 XR    RF,RF                                                            
         IC    RF,APENLEN          SUB-ELEMENT LENGTH                           
         SH    RF,=Y(APELN2Q+1)                                                 
         MVC   ATTACT,BCSPACES                                                  
         EX    RF,*+4                                                           
         MVC   ATTACT(0),APENACT   ACCOUNT                                      
         MVC   ATTSTAT,APENSTAT    STATUS                                       
         LA    R2,APENTRY+APELN2Q+1(RF)                                         
         SPACE 1                                                                
         LA    RF,IOKEY                                                         
         USING ACTRECD,RF                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,ATTACT                                                   
         GOTO1 AIO,IORD+IOACCMST+IO2                                            
         BNE   PSTANA11                                                         
         L     RF,AIO2                                                          
         GOTO1 PSTNAME,ACTRFST                                                  
         MVC   ATTACTN(L'ATTACTN),BOWORK1 ACCOUNT NAME                          
         SPACE 1                                                                
PSTANA11 LA    R5,ATTLSTQ(R5)      NEXT TABLE ENTRY                             
         BCT   R4,PSTANA10                                                      
         L     R2,BOADDR1                                                       
         B     PSTANA04                                                         
         DROP  R1,R2,R3,R5,RF                                                   
         SPACE 1                                                                
SUBLSTD  DSECT                     ** SUBSIDIARY CASH INFO TABLE **             
SUBAMNT  DS    PL6                 AMOUNT                                       
SUBTYPE  DS    XL1                 TYPE                                         
SUBLSTQ  EQU   *-SUBLSTD                                                        
SUBMAX   EQU   10                  MAXIMUM NUMBER OF ENTRIES                    
         SPACE 1                                                                
ATTLSTD  DSECT                     ** ATTRIBUTE ACCOUNT TABLE **                
ATTSTAT  DS    XL1                 STATUS                                       
ATTSDR   EQU   X'80'               TRANSACTION IS A DEBIT                       
ATTACT   DS    XL(L'ACTKULA)       ACCOUNT CODE                                 
ATTACTN  DS    CL(L'NAMEREC)       ACCOUNT NAME                                 
ATTLSTQ  EQU   *-ATTLSTD                                                        
ATTMAX   EQU   10                  MAXIMUM NUMBER OF ENTRIES                    
BAT62    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY ATTRIBUTE ACCOUNTS AND SUBSIDIARY CASH INFO                 *         
***********************************************************************         
         SPACE 1                                                                
PSTANA12 LA    R4,PDATTLST         R4=A(ATTRIBUTE LIST)                         
         USING ATTLSTD,R4                                                       
         LA    R2,PTXLIN1H                                                      
         ST    R2,BOADDR2          BOADDR2=A(HEADING LINE HEADER)               
         LA    R2,PTXLIN1          R2=A(HEADING LINE)                           
         LA    R3,AS$ATTAC                                                      
         BAS   RE,PSTCOLHD         SET UP HEADINGS                              
*                                                                               
         XC    BOBYTE1,BOBYTE1     LINE COUNTER                                 
PSTANA14 OC    ATTACT,ATTACT                                                    
         BZ    PSTANA18            END OF TABLE ENTRIES                         
         TM    ATTSTAT,ATTSDR      DEBIT ACCOUNT?                               
         BZ    PSTANA16                                                         
         MVC   BOWORK2(L'BC@DR),BC@DR                                           
         BAS   RE,PSTADC           FIT ATT ACC DETAILS ONTO LINE                
PSTANA16 LA    R4,ATTLSTQ(R4)      NEXT ATT TABLE ENTRY                         
         B     PSTANA14                                                         
*                                                                               
PSTANA18 L     R2,BOADDR2                                                       
         LA    R2,ANASTDC+ANASTDS(R2)                                           
         LA    R4,PDATTLST         START FROM TOP AGAIN                         
         XC    BOBYTE1,BOBYTE1     LINE COUNTER                                 
PSTANA20 OC    ATTACT,ATTACT                                                    
         BZ    PSTANA24            END OF TABLE ENTRIES                         
         TM    ATTSTAT,ATTSDR      CREDIT ACCOUNT?                              
         BO    PSTANA22                                                         
         MVC   BOWORK2(L'BC@DR),BC@CR                                           
         BAS   RE,PSTADC           FIT ATT ACC DETAILS ONTO LINE                
PSTANA22 LA    R4,ATTLSTQ(R4)      NEXT ATT TABLE ENTRY                         
         B     PSTANA20                                                         
*                                                                               
PSTANA24 LA    R4,PDSUBLST         R4=A(SUB-CASH LIST)                          
         USING SUBLSTD,R4                                                       
         LA    R2,PTXLIN1H                                                      
         LA    RF,PDALINM+1                                                     
         LA    R2,L'PTXLIN1H+L'PTXLIN1(R2)                                      
         BCT   RF,*-4                                                           
         ST    R2,BOADDR2           BOADDR2=A(HEADING LINE HEADER)              
         LA    R2,L'PTXLIN1H(R2)    R2=A(HEADING LINE)                          
         LA    R3,AS$SUBCH                                                      
         BAS   RE,PSTCOLHD          SET UP HEADINGS                             
*                                                                               
         LA    R0,L'PTXLIN1         LINE LENGTH                                 
         XC    BOBYTE1,BOBYTE1     LINE COUNTER                                 
PSTANA28 OC    SUBAMNT,SUBAMNT                                                  
         BZ    PSTANAX             END OF SUB-CASH LIST                         
         LA    R3,ANATAB           R3=A(TRANSACTION ANALYSIS TABLE)             
         USING ANATABD,R3                                                       
PSTANA30 CLI   ANATABD,EOT                                                      
         BE    PSTANA40                                                         
         CLC   ANATYPE,SUBTYPE     MATCH ON SCIELD TYPE                         
         BE    PSTANA34                                                         
PSTANA32 LA    R3,ANATABL(R3)                                                   
         B     PSTANA30                                                         
*                                                                               
PSTANA34 OC    ANACTRY,ANACTRY     CHECK COUNTRY STATED                         
         BZ    PSTANA35                                                         
         TM    ANACTRY,CTRYNOT                                                  
         BO    *+18                                                             
         CLC   ANACTRY,CUCTRY                                                   
         BNE   PSTANA32                                                         
         B     PSTANA35                                                         
         CLC   ANACTRY,CUCTRY                                                   
         BE    PSTANA32                                                         
*                                                                               
PSTANA35 CLC   ANAUNLG,BCSPACES    CHECK UNIT/LEDGER STATED                     
         BE    *+14                                                             
         CLC   ANAUNLG,PDUNLD                                                   
         BNE   PSTANA32                                                         
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,ANATEXT                                                     
         LA    RF,TWAD(RF)         TEXT                                         
         XR    RE,RE                                                            
         IC    RE,ANALTEXT         L'TEXT                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)       MOVE TEXT TO LINE                            
         LA    R5,ANASTDT+1(R2)    LINE POSITION FOR AMT                        
*                                                                               
*&&US*&& CURED SUBAMNT,(12,(R5)),2,MINUS=YES                                    
*&&UK*&& CURED SUBAMNT,(12,(R5)),2,FLOAT=-                                      
*                                                                               
PSTANA38 LA    RE,ANASTDC+ANASTDS                                               
         SR    R0,RE               LINE LENGTH REMAINING                        
         AR    R2,RE               SET FOR NEXT SUB TYPE                        
         LA    RF,ANASTDC                                                       
         CR    R0,RF               CHECK NEXT ONE FITS                          
         BNL   PSTANA40                                                         
         LA    RF,PDSLINM          MAXIMUM NUMBER OF LINES                      
         XR    R1,R1                                                            
         IC    R1,BOBYTE1                                                       
         CR    R1,RF                                                            
         BNL   PSTANAX             TRANSMIT SCREEN IF LINES FULL                
         L     R2,BOADDR2                                                       
         LA    R2,L'PTXLIN1H+L'PTXLIN1(R2)  NEXT SUB TYPE LINE                  
         ST    R2,BOADDR2                                                       
         LA    R1,1(R1)            INCREMENT LINE TOTAL                         
         STC   R1,BOBYTE1                                                       
         LA    R0,L'PTXLIN1        RESET LINE LENGTH                            
PSTANA40 LA    R4,SUBLSTQ(R4)      NEXT SUBLST TABLE ENTRY                      
         B     PSTANA28                                                         
*                                                                               
PSTANAX  B     PSTTRANS                                                         
         DROP  R3,R4,RC                                                         
         SPACE 1                                                                
ANATABD  DSECT                     ** TRANSACTION ANALYSIS TABLE **             
ANATYPE  DS    AL1                 SCIELD TYPE                                  
ANACTRY  DS    AL1                 COUNTRY                                      
ANAUNLG  DS    CL2                 UNIT/LEDGER                                  
ANALTEXT DS    AL1                 LENGTH OF TEXT                               
ANATEXT  DS    AL2                 DISPLACEMENT TO TEXT                         
ANATABL  EQU   *-ANATABD                                                        
         SPACE 1                                                                
ANASTDC  EQU   37                  STANDARD COLUMN WIDTH                        
ANASTDT  EQU   24                  STANDARD TEXT WIDTH                          
ANASTDS  EQU   4                   STANDARD SEPARATOR WIDTH                     
         SPACE 1                                                                
PDWORKD  DSECT                                                                  
PDGTXSUB DS    A                   A(GETTXT SUBSTITUTION TABLE)                 
         ORG   PDGTXSUB                                                         
PDGTXSEX DS    A                   A(GETTXT SUB-TABLE EXTENSION)                
PDLENSEX DS    XL1                 LENGTH OF TABLE EXTENSION                    
PDUNLD   DS    CL2                 UNIT/LEDGER                                  
PDNARLEN DS    XL1                 NARRATIVE LENGTH                             
PDPRDNLN EQU   15                  FIXED LENGTH NARRATIVE FOR W/C 99            
PDSTALEN EQU   09                  DEFAULT LENGTH FOR STATUS WORD               
*                                                                               
PDDRAFT  DS    XL2                 DRAFTED ON DDMMMYY                           
PDEFF    DS    XL2                 EFFECTIVE ON DDMMMYY                         
PDLIVE   DS    XL2                 LIVE ON DDMMMYY                              
PDDUE    DS    XL2                 DUE ON DDMMMYY                               
PDPOSTMA DS    XL2                 POSTING MOA MMMYY                            
PDREVD   DS    XL2                 REVERSED ON DDMMMYY                          
PDREVDMA DS    XL2                 REVERSING MOA MMMYY                          
PDUSED   DS    XL2                 MARKED USED ON DDMMMYY                       
PDUSEDMA DS    XL2                 MARKED USED MOA MMMYY                        
PDUPDAT  DS    XL2                 UPDATED ON DDMMMYY                           
PDPEEL   DS    XL2                 PEELED ON DDMMMYY                            
PDOLACT  DS    XL3                 LAST ACTIVITY ON DDMMMYY                     
*                                                                               
PDCURC   DS    CL3                 CURRENCY CODE                                
PDXRAT   DS    CL10                EXCHANGE RATE                                
PDCAMT   DS    CL15                AMOUNT IN CURRENCY                           
*                                                                               
PDSUNDRY DS    XL1                 SUNDRY CREDITOR SWITCH                       
PDSCDADN DS    XL1                                 NO. OF ADDRESS LINES         
PDSCDAD1 DS    CL26                                ADDRESS LINE 1               
PDSCDAD2 DS    CL26                                        LINE 2               
PDSCDAD3 DS    CL26                                        LINE 3               
PDSCDAD4 DS    CL26                                        LINE 4               
PDSCDAD5 DS    CL26                                        LINE 5               
*                                                                               
PDBATTYP DS    XL1                                                              
PDRECTYP DS    XL1                                                              
PDSORDET DS    XL16                                                             
PDRECDET DS    XL(RECDETQ)                                                      
PDORDDET DS    XL(ORDDETQ)                                                      
PDPAYLST DS    (PDMPAYM)XL(PYLISTQ)                                             
PDSUBLST DS    (SUBMAX)XL(SUBLSTQ)                                              
PDATTLST DS    (ATTMAX)XL(ATTLSTQ)                                              
*                                                                               
PDNARWRK DS    (PDNLINM)XL(L'PTXLIN1)                                           
*                                                                               
PDDLINM  EQU   5                   MAX NO. OF DATE LINES                        
PDOLINM  EQU   4                   MAX NO. OF OTHER INFO LINES                  
PDNLINM  EQU   3                   MAX NO. OF NARRATIVE LINES                   
PDALINM  EQU   5                   MAX NO. OF ATTRIBUTE ACCOUNT LINES           
PDSLINM  EQU   5                   MAX NO. OF SUB-CASH INFO LINES               
PDCLINM  EQU   2                   MAX NO. OF S/CREDITOR ADDRESS LINES          
PDMPAYM  EQU   6                   MAX NO. OF MANUAL PAYMENTS                   
BAT62    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY COLUMN HEADINGS (ANALYSIS SCREEN)                           *         
***********************************************************************         
         SPACE 1                                                                
PSTCOLHD ST    RE,BOADDR1                                                       
         LA    R1,BOWORK1                                                       
         USING GETTXTD,R1                                                       
         XC    BOWORK1(L'BOWORK1+L'BOWORK2),BOWORK1                             
         GOTO1 VGETTXT,(R1),(R3),('ANASTDC',BOWORK2),('GTMSCR',0),,    X        
               ('GT1OWRK',0)                                                    
         LA    RF,ANASTDC                                                       
         BCTR  RF,0                                                             
         EX    RF,*+16               FIRST COLUMN                               
         LA    R2,ANASTDC+ANASTDS(R2)                                           
         EX    RF,*+8                SECOND COLUMN                              
         B     *+10                                                             
         MVC   0(0,R2),BOWORK2                                                  
         L     R2,BOADDR2                                                       
         NI    FVATRB-FVIHDR(R2),FVFOK-FVAHIGH NON-INTENSIFIED FIELD            
         LA    R2,L'PTXLIN1H+L'PTXLIN1H+L'PTXLIN1(R2)                           
         ST    R2,BOADDR2            FIRST LINE FOR DATA                        
         L     RE,BOADDR1                                                       
         BR    RE                                                               
         SPACE 1                                                                
***********************************************************************         
* DISPLAY OTHER INFORMATION HEADINGS (DISPLAY SCREEN)                 *         
***********************************************************************         
         SPACE 1                                                                
PSTOTHHD ST    RE,BOADDR1                                                       
         LA    R1,BOPARM                                                        
         USING GETTXTD,R1                                                       
         XC    BOPARM,BOPARM                                                    
         GOTO1 VGETTXT,(R1),(R3),(L'PTXLIN1,BOWORK2),('GTMSCR',0),,    X        
               ('GT1OWRK',0)                                                    
         XR    RF,RF                                                            
         IC    RF,GTMAXL           OUTPUT LENGTH                                
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R2),BOWORK2                                                  
         SH    R2,=Y(L'PTXLIN1H)                                                
         NI    FVATRB-FVIHDR(R2),FVFOK-FVAHIGH NON-INTENSIFIED FIELD            
         LA    R2,L'PTXLIN1H+L'PTXLIN1H+L'PTXLIN1(R2)                           
         ST    R2,BOADDR2          FIRST LINE FOR DATA                          
         XR    R1,R1               INCREMENT LINE TOTAL                         
         IC    R1,BOBYTE1                                                       
         LA    R1,1(R1)                                                         
         STC   R1,BOBYTE1                                                       
         L     RE,BOADDR1                                                       
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY OTHER INFORMATION DETAILS (DISPLAY SCREEN)                  *         
***********************************************************************         
         SPACE 1                                                                
PSTOTHDT ST    RE,BOADDR1                                                       
         LA    R1,BOPARM                                                        
         USING GETTXTD,R1                                                       
         XC    BOPARM,BOPARM                                                    
         GOTO1 VGETTXT,(R1),(R3),(L'PTXLIN1,(R2)),('GTMSCR',0),,       X        
               BOWORK1                                                          
         XR    R1,R1               INCREMENT LINE TOTAL                         
         IC    R1,BOBYTE1                                                       
         LA    R1,1(R1)                                                         
         STC   R1,BOBYTE1                                                       
         L     RE,BOADDR1                                                       
         BR    RE                                                               
         DROP  R1                                                               
         SPACE 1                                                                
***********************************************************************         
* ATTRIBUTE ACCOUNT DETAILS CONSTRUCTION (ANALYSIS SCREEN)            *         
***********************************************************************         
         SPACE 1                                                                
         USING ATTLSTD,R4                                                       
PSTADC   XR    R1,R1                                                            
         IC    R1,BOBYTE1                                                       
         LA    R1,1(R1)            INCREMENT LINE TOTAL                         
         LA    RF,PDALINM                                                       
         CR    R1,RF                                                            
         BHR   RE                                                               
         STC   R1,BOBYTE1                                                       
         ST    RE,BOADDR1                                                       
         MVC   BOWORK1,BCSPACES                                                 
         MVC   BOWORK1(L'BC@DR),BOWORK2                                         
         MVC   BOWORK1+L'BC@DR(L'ATTACT),ATTACT                                 
         MVC   BOWORK1+L'BC@DR+L'ATTACT+1(L'ATTACTN),ATTACTN                    
         LA    R3,L'BC@DR+L'ATTACT+1+L'ATTACTN                                  
         GOTO1 VSQUASH,BOPARM,BOWORK1,(R3)                                      
         MVC   0(ANASTDC,R2),BOWORK1                                            
         LA    R2,L'PTXLIN1H+L'PTXLIN1(R2)  NEXT ATT ACC LINE                   
         L     RE,BOADDR1                                                       
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* TEST FOR SUNDRY CREDITOR ACCOUNT (DISPLAY/ANALYSIS SCREENS)         *         
***********************************************************************         
         SPACE 1                                                                
*&&UK                                                                           
         USING ACTRECD,R3                                                       
PSTSCD   L     R3,AIO1                                                          
         LA    R3,ACTRFST                                                       
         XR    RF,RF                                                            
         USING RSTELD,R3                                                        
PSCD02   CLI   RSTEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   RSTEL,RSTELQ                                                     
         BNE   PSCD04                                                           
         CLI   RSTEL,RSTLN3Q                                                    
         BL    PSCDN                                                            
         TM    RSTSTAT5,RSTSSUND                                                
         BZ    PSCDN                                                            
         B     PSCDY                                                            
PSCD04   IC    RF,RSTLN                                                         
         AR    R3,RF                                                            
         B     PSCD02                                                           
PSCDN    LTR   RE,RE                                                            
         B     *+6                                                              
PSCDY    CR    RE,RE                                                            
         BR    RE                                                               
         DROP  R3                                                               
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* STATUS LINE CONSTRUCTION (DISPLAY/ANALYSIS SCREENS)                 *         
* NTRY - P1=L'STATUS WORD-1/A(STATUS WORD) - COMPULSORY               *         
*      - P2=L'APPENDAGE-1/A(APPENDAGE) - OPTIONAL                     *         
* EXIT - R2=A(NEXT SPACE ON STATUS LINE)                              *         
***********************************************************************         
         SPACE 1                                                                
PSTSLC   NTR1                                                                   
         XR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         L     RF,0(R1)                                                         
         XR    R3,R3                                                            
         IC    R3,4(R1)                                                         
         L     R4,4(R1)                                                         
         LH    R0,BOHALF1                                                       
PSTSLC2  SR    R0,RE               CHECK ENOUGH ROOM TO DISPLAY WORD            
         SR    R0,R3                                                            
         BP    PSTSLC4                                                          
         LA    R0,BOWORK2                                                       
         CR    R2,R0                                                            
         BNL   PSTSLCX             BOTH STATUS LINES ARE FULL                   
         LA    R2,BOWORK2                                                       
         LA    R0,L'PTXSTA2                                                     
         B     PSTSLC2                                                          
PSTSLC4  EX    RE,*+4                                                           
         MVC   0(0,R2),0(RF)       MOVE IN STATUS WORD                          
         LR    RF,R2                                                            
         AR    RF,RE                                                            
         CLI   0(RF),C' '                                                       
         BH    PSTSLC6                                                          
         CR    RF,R2                                                            
         BE    *+8                                                              
         BCT   RF,*-14                                                          
         DC    H'0'                                                             
PSTSLC6  CR    RE,R3               MOVE IN APPENDAGE                            
         BE    PSTSLC8                                                          
         LTR   R3,R3                                                            
         BZ    PSTSLC8                                                          
         LA    R2,1(RF)                                                         
         LR    RE,R3                                                            
         LR    RF,R4                                                            
         B     PSTSLC4                                                          
PSTSLC8  BCTR  R0,0                                                             
         MVC   1(L'BCCOMMA,RF),BCCOMMA                                          
         LA    R2,1+L'BCCOMMA(RF)  BUMP ALONG FOR NEXT INSERTION                
         STH   R0,BOHALF1                                                       
PSTSLCX  XIT1  REGS=(R2)                                                        
         SPACE 1                                                                
***********************************************************************         
* BATCH DETAILS LINE CONSTRUCTION (DISPLAY/ANALYSIS SCREENS)          *         
***********************************************************************         
         SPACE 1                                                                
PSTBDCA  EX    R1,*+4                                                           
         MVC   0(0,R3),0(RF)                                                    
PSTBDCB  LR    RF,R3                                                            
         AR    RF,R1                                                            
         CLI   0(RF),C' '                                                       
         BH    PSTBDCX                                                          
         CR    RF,R3                                                            
         BER   RE                                                               
         BCT   RF,*-12                                                          
         DC    H'0'                                                             
PSTBDCX  MVC   1(L'BCCOMMA,RF),BCCOMMA                                          
         LA    R3,1+L'BCCOMMA(RF)                                               
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* EXTRACT NAME FROM A RECORD (DISPLAY/ANALYSIS SCREENS)               *         
***********************************************************************         
         SPACE 1                                                                
         USING NAMELD,R1                                                        
PSTNAME  XR    RF,RF                                                            
         MVC   BOWORK1,BCSPACES                                                 
PSTNAME2 CLI   NAMEL,0                                                          
         BER   RE                                                               
         IC    RF,NAMLN                                                         
         CLI   NAMEL,CACELQ                                                     
         BE    PSTNAME4                                                         
         CLI   NAMEL,NAMELQ                                                     
         BE    *+10                                                             
         AR    R1,RF                                                            
         B     PSTNAME2                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   BOWORK1(0),NAMEREC                                               
*                                                                               
         USING CACELD,R1                                                        
PSTNAME4 SH    RF,=Y(CACLN1Q+1)                                                 
         BMR   RE                                                               
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   BOWORK1(0),CACNAME                                               
         DROP  R1                                                               
         SPACE 1                                                                
***********************************************************************         
* TRANSMIT DISPLAY/ANALYSIS SCREEN                                    *         
***********************************************************************         
         SPACE 1                                                                
PSTTRANS LA    R1,BASMSGH                                                       
         LA    RF,OSVALS-1                                                      
         SR    RE,RE                                                            
         ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    *+12                                                             
         OI    FVOIND-FVIHDR(R1),FVOXMT  TRANSMIT                               
         BXLE  R1,RE,*-12                                                       
         MVC   FVMSGNO,=AL2(AI$RDENX)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LIST POSTING ELEMENTS FOR A BATCH ITEM                              *         
***********************************************************************         
         SPACE 1                                                                
         USING PLWORKD,RC                                                       
PSTLST   TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BZ    ERRINVAS                                                         
         LA    RF,PLOPTS                                                        
         ST    RF,AOVEROUT         SAVE A(OUTPUT AREA FOR OPTIONS)              
         MVC   PLOPTS,BCSPACES                                                  
         L     R1,=A(PLOVAL)                                                    
         A     R1,BORELO                                                        
         ST    R1,AOVERVAL         SAVE A(OPTION VALIDATION ROUTINES)           
         MVC   PLODIS,BCSPACES                                                  
         GOTO1 AFVAL,BASOPTH                                                    
         GOTO1 AVALOPT,PLOTAB      VALIDATE OPTIONS                             
         BNE   EXIT                                                             
*                                                                               
         LA    R2,CSLSTCUR                                                      
         USING LSTTABD,R2          R2=A(LIST TABLE ENTRY)                       
         CLI   TWASCRN,PLSTSCRN                                                 
         BE    PSTLST02                                                         
         GOTO1 AOVRSCR,BCPARM,('PLSTSCRN',BASOLY1H)                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   CSLTINDS,CSLTIFST   SET FIRST FOR LIST                           
         MVC   PLSEQ,LSTISEQ       EXTRACT ITEM VALUES                          
         MVC   PLREF,LSTIREF                                                    
         MVC   PLDATE,LSTIDATE                                                  
         MVI   PLNARRL,0                                                        
         MVI   PLNARR,C' '                                                      
         MVC   PLNARR+1(L'PLNARR-1),PLNARR                                      
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* HANDLE LIST SUB-ACTIONS                                             *         
***********************************************************************         
         SPACE 1                                                                
PSTLST02 LA    R3,CSLSTCUR                                                      
         USING LSTTABD,R3          R3=A(LIST TABLE)                             
         NI    CSLTINDS,FF-(CSLTIHLD+CSLTIANY)                                  
*                                                                               
         CLC   PLOPTS(PLVALSL),PLVALS                                           
         BE    *+8                                                              
         OI    CSLTINDS,CSLTIHLD   HOLD PAGE IF DISPLAY OPTIONS CHANGE          
*                                                                               
         MVC   PLVALS(PLVALSL),PLOPTS                                           
*                                                                               
         TM    CSLTINDS,CSLTIFST   TEST CHANGE OF KEY FIELDS                    
         BNZ   PSTLST31                                                         
*                                                                               
         TM    PLNARFLG,PLNARCHG   NARRATIVE CHANGE ALLOWED?                    
         BZ    PSTLST04                                                         
         OI    PLNARFLG,PLNARINP   SET TO WRITE BACK INPUT                      
         BAS   RE,PSTNAR                                                        
*                                                                               
PSTLST04 MVC   LSTTRECN,CSPAG#LO                                                
         SR    R0,R0                                                            
         ICM   R0,1,CSLSTNUM       R0=NUMBER OF LIST ENTRIES                    
         BZ    PSTLST31                                                         
         LA    R2,PLIACT1H                                                      
         USING PLIACT1H,R2                                                      
*                                                                               
PSTLST06 NI    PLFLAG,FF-PLFINPT   CLEAR USER INPUT FLAG BIT                    
         CLI   PLIACT1H+(FVILEN-FVIHDR),0                                       
         BE    PSTLST07                                                         
         TM    PLIACT1H+(FVIIND-FVIHDR),FVITHIS  INPUT THIS TIME?               
         BZ    PSTLST07                                                         
         OI    PLFLAG,PLFINPT      SET USER INPUT                               
         B     PSTLST08                                                         
PSTLST07 XC    PLIACT1,PLIACT1                                                  
         OI    PLIACT1H+(FVOIND-FVIHDR),FVOXMT                                  
         TM    CSLTINDS,CSLTIEOL+CSLTIEOP                                       
         BZ    PSTLST26                                                         
         CLC   LSTTRECN,CSSEL#LO                                                
         BL    PSTLST26                                                         
         CLC   LSTTRECN,CSSEL#HI                                                
         BH    PSTLST26                                                         
         SR    R4,R4                                                            
         ICM   R4,3,CSSELMUL                                                    
         A     R4,AOVERSEL                                                      
         USING SELTABD,R4          R4=A(SELECT TABLE ENTRY)                     
         SR    RE,RE                                                            
         ICM   RE,3,SELTDSPN                                                    
         LA    RE,TWAD(RE)                                                      
         MVC   PLIACT1,0(RE)                                                    
*                                                                               
PSTLST08 GOTO1 AFVAL,PLIACT1H                                                   
         BNE   PSTLST26                                                         
         L     R4,AOVERSEL         R4=A(SELECT TABLE)                           
         CLI   FVIFLD,C'*'                                                      
         BE    PSTLST26                                                         
         CLI   FVIFLD,C'-'                                                      
         BNE   *+12                                                             
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         B     PSTLST26                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FVXLEN                                                      
         BZ    PSTLST10                                                         
         LA    RE,FVIFLD(RF)       POINT TO END OF INPUT FIELD                  
         CLI   0(RE),C'+'                                                       
         BE    *+12                                                             
         CLI   0(RE),C'&&'                                                      
         BNE   PSTLST10                                                         
         MVC   CSSEL#LO,LSTTRECN   SET LOW RECORD NUMBER                        
         MVC   CSSEL#HI,BCEFFS     SET DEFAULT HIGH VALUE                       
         LA    R1,CSLTIEOL                                                      
         CLI   0(RE),C'&&'                                                      
         BE    *+14                                                             
         MVC   CSSEL#HI,CSPAG#HI                                                
         LA    R1,CSLTIEOP                                                      
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         EX    R1,*+4                                                           
         OI    CSLTINDS,0                                                       
         BCTR  RF,0                                                             
         STC   RF,FVXLEN                                                        
*                                                                               
PSTLST10 CLI   SELTABD,EOT                                                      
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,SELTDSPN                                                    
         LA    RE,TWAD(RE)                                                      
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(RE)                                                  
         BNE   PSTLST16                                                         
         GOTO1 ATSTMIX,SELTPARM    VALIDATE RECORD/ACTION                       
         BNE   PSTLST16                                                         
*                                                                               
         CLC   LSTTRECN,CSSEL#LO   TEST SELECT MULTIPLE INPUT LINE              
         BNE   PSTLST12                                                         
         L     RE,AOVERSEL                                                      
         LA    RF,SELTABD                                                       
         SR    RF,RE                                                            
         STCM  RF,3,CSSELMUL       SET DISPLACEMENT TO SELTAB ENTRY             
*                                                                               
PSTLST12 GOTO1 ATSARIO,TSAGET      GET POSTING LIST RECORD                      
*                                                                               
         MVC   PLMASK,SELTMASK     TEST LINE ACTION IS VALID                    
         NC    PLMASK,LSTTMASK                                                  
         CLC   PLMASK,SELTMASK                                                  
         BE    PSTLST18                                                         
*                                                                               
PSTLST14 CLC   LSTTRECN,CSSEL#LO                                                
         BE    PSTLST26                                                         
         BL    PSTLST16                                                         
         TM    PLFLAG,PLFINPT      TEST INPUT THIS TIME                         
         BZ    PSTLST26                                                         
*                                                                               
PSTLST16 LA    R4,SELTABL(R4)                                                   
         B     PSTLST10                                                         
*                                                                               
PSTLST18 MVC   CSLSTCUR,LSTTABD                                                 
         OI    CSLTINDS,CSLTIANY   SET LINE PROCESSED THIS SCREEN               
         OC    SELTREC(L'SELTREC+L'SELTACT),SELTREC                             
         BZ    PSTLST20                                                         
         L     RE,ATWA                                                          
         LA    RF,PLIACT1H                                                      
         SR    RF,RE                                                            
         STCM  RF,3,CSSELACT       SET DISPLACEMENT TO FIELD HEADER             
         L     RE,AOVERSEL                                                      
         LA    RF,SELTABD                                                       
         SR    RF,RE                                                            
         STCM  RF,3,CSSELCUR       SET DISPLACEMENT TO SELTAB ENTRY             
         CLC   CSSEL#LO,LSTTRECN                                                
         BNE   *+8                                                              
         STCM  RF,3,CSSELMUL       SET DISPLACEMENT TO MULTI-ENTRY              
         STC   R0,CSSELREM         SET NUMBER OF LINES REMAINING                
         GOTO1 ANTRSES,SELTPARM                                                 
         EJECT                                                                  
***********************************************************************         
* LINE ACTION COMPLETED                                               *         
***********************************************************************         
         SPACE 1                                                                
PSTLSTR1 SR    R2,R2                                                            
         ICM   R2,3,CSSELACT                                                    
         A     R2,ATWA             R2=A(ACTION LINE)                            
         LA    R3,CSLSTCUR         R3=A(LIST ENTRY)                             
         SR    R4,R4                                                            
         ICM   R4,3,CSSELCUR                                                    
         A     R4,AOVERSEL         R4=A(SELECT TABLE ENTRY)                     
         SR    R0,R0                                                            
         ICM   R0,1,CSSELREM       R0=NUMBER OF LINES REMAINING                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     PSTLST22                                                         
*                                                                               
PSTLST20 LA    RF,BAT62                                                         
         ICM   RF,8,SELTRTN                                                     
         BASR  RE,RF                                                            
*                                                                               
PSTLST22 MVI   PLIACT1,C'*'                                                     
         SR    RE,RE                                                            
         ICM   RE,3,SELTDSPN                                                    
         LA    RE,TWAD(RE)                                                      
         MVC   PLIACT1+1(L'PLIACT1-1),0(RE)                                     
         GOTO1 ATSARIO,TSAPUT                                                   
         OI    PLIACT1H+(FVOIND-FVIHDR),FVOXMT                                  
         GOTO1 BLDLIN,PLILIN1                                                   
         CLI   BCPFKEY,PFKQUITQ                                                 
         BNE   PSTLST26                                                         
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         B     PSTLST30                                                         
*                                                                               
PSTLST26 LA    R2,PLIACT2H                                                      
         SR    R1,R1                                                            
         ICM   R1,3,LSTTRECN                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,LSTTRECN                                                    
         BCT   R0,PSTLST06                                                      
         BCTR  R1,0                                                             
         STCM  R1,3,LSTTRECN                                                    
*                                                                               
         TM    CSLTINDS,CSLTIEOF   TEST END OF FILE REACHED                     
         BZ    PSTLST28                                                         
         CLC   LSTTRECN,CSHIRECN   TEST THIS IS LAST RECORD                     
         BNE   PSTLST28                                                         
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         DROP  R2                                                               
*                                                                               
PSTLST28 TM    CSLTINDS,CSLTIANY                                                
         BZ    PSTLST31                                                         
         TM    CSLTINDS,CSLTIEOL                                                
         BNZ   PSTLST31                                                         
*                                                                               
PSTLST30 CLC   PLODIS,BCSPACES     ARE ALTERNATIVE DISPLAY OPTIONS SET?         
         BNE   PSTLST53            YES - REDISPLAY ENTIRE SCREEN                
         BAS   RE,PSTDIN           REDISPLAY NARRATIVE ONLY                     
         LA    R0,PLIACT1H                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$ACTOK)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* HANDLE SCROLLING                                                    *         
***********************************************************************         
         SPACE 1                                                                
PSTLST31 MVC   PLPAG#LO,CSPAG#LO   SAVE LAST LOW & HIGH RECORDS                 
         MVC   PLPAG#HI,CSPAG#HI                                                
         XC    CSPAG#LO,CSPAG#LO   CLEAR LOW & HIGH VALUES FOR PAGE             
         XC    CSPAG#HI,CSPAG#HI                                                
         TM    CSLTINDS,CSLTIFST   TEST FIRST FOR LIST                          
         BZ    PSTLST32                                                         
         MVI   CSLTINDS,0          RESET LIST INDICATORS                        
         MVI   CSLSTNUM,0          CLEAR NUMBER OF LIST ENTRIES                 
         MVC   CSHIRECN,CSPSRECN   RESET HIGH RECORD NUMBER                     
         MVI   PLNARFLG,PLNARFST   SET FIRST FOR NARRATIVE CHANGE CHECK         
         B     PSTLST44                                                         
*                                                                               
PSTLST32 CLI   BCSCROLL,0          TEST SCROLL SPECIFIED                        
         BNE   PSTLST33                                                         
         MVI   BCSCROLL,PFKIPAGE   NO - SET SCROLL DOWN A PAGE                  
         TM    CSLTINDS,CSLTIHLD   TEST HOLD PAGE IF DISPLAY CHANGED            
         BZ    PSTLST33                                                         
         MVC   CSPAG#LO,PLPAG#LO   RESTORE LAST LOW & HIGH VALUES               
         MVC   CSPAG#HI,PLPAG#HI                                                
         B     PSTLST52                                                         
*                                                                               
PSTLST33 TM    BCSCROLL,PFKIUPDN   TEST SCROLL UP OR DOWN                       
         BZ    PSTLST36                                                         
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,CSPSRECN                                                    
         TM    BCSCROLL,PFKIMAXN   TEST SCROLL FIRST                            
         BNZ   PSTLST34                                                         
         LA    RF,PLISTMAX         SCROLL UP (BACKWARDS)                        
         TM    BCSCROLL,PFKIHALF                                                
         BZ    *+8                                                              
         SRL   RF,1                                                             
         LA    RF,1(RF)                                                         
         SR    RE,RE                                                            
         ICM   RE,3,PLPAG#LO                                                    
         SR    RE,RF               BACK-UP TO RECORD NUMBER-1                   
         BM    *+12                                                             
         CLM   RE,3,CSPSRECN       TEST NOT < LOW RECORD FOR SESSION            
         BNL   PSTLST34                                                         
         SR    RE,RE               SET TO START FROM LOW RECORD                 
         ICM   RE,3,CSPSRECN                                                    
*                                                                               
PSTLST34 STCM  RE,3,LSTTRECN                                                    
         MVI   CSLSTNUM,0                                                       
         B     PSTLST40                                                         
*                                                                               
PSTLST36 SR    R1,R1                                                            
         ICM   R1,1,CSLSTNUM       PICK UP NUMBER OF ENTRIES IN PAGE            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   CSLSTNUM,0                                                       
         TM    BCSCROLL,PFKIHALF   TEST HALF PAGE SCROLL                        
         BZ    *+8                                                              
         SRL   R1,1                                                             
         SH    R1,=H'1'                                                         
         BM    PSTLST38                                                         
         SR    R0,R0                                                            
         ICM   R0,3,PLPAG#LO                                                    
         AR    R1,R0                                                            
         STCM  R1,3,LSTTRECN                                                    
         TM    CSLTINDS,CSLTIEOF   TEST END OF FILE ENCOUNTERED                 
         BZ    PSTLST40                                                         
         CLC   LSTTRECN,CSHIRECN   TEST LAST RECORD DISPLAYED                   
         BL    PSTLST40                                                         
*                                                                               
PSTLST38 MVC   LSTTRECN,CSPSRECN   SET TO DISPLAY FIRST PAGE                    
*                                                                               
PSTLST40 SR    RE,RE               BUMP TO NEXT RECORD                          
         ICM   RE,3,LSTTRECN                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,LSTTRECN                                                    
         CLC   LSTTRECN,CSHIRECN   TEST END OF FILE                             
         BH    PSTLST52                                                         
         GOTO1 LSTADD,0            ADD ENTRY TO LSTTAB                          
         BE    PSTLST40                                                         
         B     PSTLST52                                                         
*                                                                               
PSTLST44 TM    PLNARFLG,PLNARFST   1ST TIME FOR NARRATIVE CHANGE CHECK          
         BNO   *+8                                                              
         BAS   RE,PSTNAR                                                        
         MVC   IODAOVER,LSTTDA     READ BATCH ITEM RECORD                       
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO1                                                          
         USING TBARECD,R2                                                       
         LA    R2,TBARFST                                                       
         USING ASKELD,R2                                                        
         SR    R0,R0                                                            
PSTLST46 CLI   ASKEL,0             TEST EOR                                     
         BE    PSTLST50                                                         
         CLI   ASKEL,ASKELQ                                                     
         BE    PSTLST47                                                         
         CLI   ASKEL,GINELQ        USE GINEL IF PRESENT                         
         BNE   PSTLST48                                                         
         MVC   PLGIN,GININV-GINELD(R2)                                          
         GOTO1 PASSGIN,PLGIN                                                    
         B     PSTLST50                                                         
PSTLST47 XC    LSTTABD(LSTTABL),LSTTABD                                         
         ICM   RE,3,CSHIRECN                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,LSTTRECN                                                    
         GOTO1 LSTADD,1                                                         
*                                                                               
PSTLST48 IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     PSTLST46                                                         
*                                                                               
PSTLST50 OI    CSLTINDS,CSLTIEOF   SET END OF FILE                              
         B     PSTLST38                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY PAGE OF DATA                                                *         
***********************************************************************         
         SPACE 1                                                                
PSTLST52 TWAXC PLIACT1H,PLINARH-1,PROT=Y                                        
         TWAXC PLINAR1H,PLIPFKH-1                                               
*                                                                               
PSTLST53 CLC   CSHIRECN,CSPSRECN   TEST ANY RECORDS FOUND                       
         BNE   *+6                                                              
         DC    H'0'                RECORDS MUST EXIST IF WE'RE HERE             
*                                                                               
         MVC   LSTTRECN,CSPAG#LO                                                
         CLC   LSTTRECN,CSSEL#HI   TEST > HIGH MULTIPLE SELECT                  
         BNH   *+14                                                             
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
         SR    R0,R0                                                            
         ICM   R0,1,CSLSTNUM       R0=NUMBER OF LINES TO DISPLAY                
         BNZ   *+6                                                              
         DC    H'0'                DIE FOR NOW IF NO DISPLAY LINES              
         LA    R2,PLIACT1H                                                      
         USING PLIACT1H,R2         R2=A(SCREEN LINE)                            
*                                                                               
PSTLST56 GOTO1 ATSARIO,TSAGET                                                   
         GOTO1 BLDLIN,PLILIN1                                                   
         MVI   PLIACT1H+(FVILEN-FVIHDR),0                                       
         LA    R2,PLIACT2H                                                      
         SR    R1,R1                                                            
         ICM   R1,3,LSTTRECN                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,LSTTRECN                                                    
         BCT   R0,PSTLST56                                                      
         BCTR  R1,0                                                             
         STCM  R1,3,LSTTRECN                                                    
         DROP  R2                                                               
*                                                                               
         CLC   LSTTRECN,CSSEL#LO   TEST < LOW MULTIPLE SELECT                   
         BNL   *+14                                                             
         XC    CSSEL#LO(L'CSSEL#LO+L'CSSEL#HI),CSSEL#LO                         
         NI    CSLTINDS,FF-(CSLTIEOP+CSLTIEOL)                                  
*                                                                               
         CURED PLSEQ,(L'PLIITE,PLIITE),0,ALIGN=LEFT                             
         MVC   PLIREF,PLREF                                                     
         GOTO1 VDATCON,BODMCB,(1,PLDATE),(17,PLIDAT)                            
*                                                                               
         TM    CSLTINDS,CSLTIEOL   TEST SELECT TO END OF LIST                   
         BNZ   PSTLST04                                                         
*                                                                               
         GOTO1 LSTVHED             SET VARIABLE COLUMN HEADING                  
*                                                                               
         BAS   RE,PSTDIN           DISPLAY NARRATIVE                            
*                                                                               
PSTLSTX  LA    R0,PLIACT1H                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$PDINX)                                           
         MVI   FVOMTYP,GTMINF                                                   
         TM    CSLTINDS,CSLTIEOF   TEST END-OF-FILE ENCOUNTERED                 
         BZ    EXIT                                                             
         CLC   LSTTRECN,CSHIRECN   TEST LAST RECORD DISPLAYED                   
         BNE   EXIT                                                             
         MVC   FVMSGNO,=AL2(AI$PODPR)                                           
         B     EXIT                                                             
         DROP  R3,RC                                                            
         SPACE 1                                                                
PLISTMAX EQU   (PLINAR1H-PLIACT1H)/(PLIACT2H-PLIACT1H)                          
         SPACE 1                                                                
PLWORKD  DSECT                     ** ITELST LOCAL W/S **                       
PLPAG#LO DS    XL2                 LOW RECORD ON CURRENT PAGE                   
PLPAG#HI DS    XL2                 HIGH RECORD ON CURRENT PAGE                  
PLFLAG   DS    XL1                 INDICATOR                                    
PLFINPT  EQU   X'80'               USER INPUT THIS TIME                         
PLGINEL  EQU   X'40'               GROUP INVOICE NO. ELEMENT FOUND              
PLGIN    DS    XL4                 GROUP INVOICE NO.                            
PLMASK   DS    XL(L'LSTTMASK)      VALID ACTION MASK WORK AREA                  
PLOPTS   DS    XL(PLVALSL)         DISPLAY OPTIONS                              
PLWORKX  EQU   *                                                                
BAT62    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* PASS GROUP INVOICE TRANSACTIONS TO LIST TABLE                       *         
* NTRY - R2=A(GROUP INVOICE ELEMENT),R3=A(CURRENT LIST ENTRY)         *         
***********************************************************************         
          SPACE 1                                                               
PASSGIN   NTR1                                                                  
          USING LSTTABD,R3                                                      
          L     RE,AIO1            RE=A(BATCH ITEM RECORD)                      
          LA    RF,IOKEY           BUILD GROUP INVOICE PASSIVE KEY              
          USING GINPASD,RF                                                      
          XC    GINPKEY,GINPKEY                                                 
          MVI   GINPTYP,GINPTYPQ                                                
          MVC   GINPCPY,TBAKCPY-TBARECD(RE)                                     
          MVC   GINPINV,0(R1)                                                   
          LA    R1,IOHI+IOACCDIR+IO2                                            
          B     *+8                                                             
PGIN02    LA    R1,IOSEQ+IOACCDIR+IO2                                           
          GOTO1 AIO                                                             
          BE    *+6                                                             
          DC    H'0'                                                            
          CLC   IOKEY(GINPISN-GINPKEY),IOKEYSAV                                 
          BNE   PGINX                                                           
          GOTO1 AIO,IOGET+IOACCMST+IO2                                          
          BE    *+6                                                             
          DC    H'0'                                                            
          MVC   BCWORK(L'IOKEY),IOKEY  SAVE PASSIVE KEY                         
          LA    RF,IOKEY                                                        
          XC    LSTTABD(LSTTABL),LSTTABD                                        
          OC    GINPISN,GINPISN    JOB/EXPENSE TXS ARE HIGHLIGHTED              
          BZ    PGIN04                                                          
          CLI   GINPPTYP,GINTMAC                                                
          BNE   PGIN04                                                          
          OI    LSTPIND1,LSTP1JEX                                               
PGIN04    L     R4,AIO2                                                         
          ICM   RE,3,CSHIRECN                                                   
          LA    RE,1(RE)                                                        
          STCM  RE,3,LSTTRECN                                                   
          GOTO1 LSTADD,2           CREATE LIST TABLE ENTRY                      
          MVC   IOKEY,BCWORK       RESTORE PASSIVE KEY                          
          GOTO1 AIO,IORD+IOACCDIR+IO2  RE-READ TO ESTABLISH SEQUENCE            
          BE    PGIN02                                                          
          DC    H'0'                                                            
PGINX     XIT1                                                                  
          DROP  RF                                                              
          EJECT                                                                 
***********************************************************************         
* ROUTINE TO CREATE AN ITEM LIST TABLE ELEMENT                        *         
*                                                                     *         
* NTRY - R1=0/POST ENTRY,1/CREATE ENTRY FORM ASKEL,2/FROM GINEL       *         
*        R2=A(ASKEL) IF R1=1,R4=A(TRNREC) IF R1=2                     *         
*        LSTTRECN IS RECORD NUMBER OR LIST ENTRY (IF R1=NON-ZERO)     *         
*        CSLSTNUM IS NUMBER OF ENTRIES IN PAGE SO FAR                 *         
* EXIT - CC=NOT EQUAL IF PAGE FULL                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING PLWORKD,RC          RC=A(LOCAL W/S)                              
LSTADD   NTR1  ,                                                                
         USING ASKELD,R2                                                        
         LA    R3,CSLSTCUR                                                      
         USING LSTTABD,R3          R3=A(CURRENT LIST ENTRY)                     
         SR    RE,RE                                                            
         ICM   RE,1,CSLSTNUM                                                    
         LA    R0,1(RE)                                                         
         CLM   R0,1,=AL1(PLISTMAX) TEST TABLE FULL                              
         BH    LSTADDN                                                          
         LTR   R1,R1                                                            
         BNZ   *+12                                                             
         STC   R0,CSLSTNUM                                                      
         B     LSTADD22                                                         
         MVI   LSTPSEQ,X'FF'                                                    
         CH    R1,=H'1'                                                         
         BNE   LSTADD03                                                         
*                                                                               
         MVC   IOKEY,ASKKEY        READ TRANSACTION RECORD                      
         GOTO1 AIO,IORD+IOACCMST+IO2                                            
         BE    LSTADD02                                                         
         BH    *+6                                                              
         DC    H'0'                CAN'T READ ACCDIR/ACCMST                     
         MVI   LSTTSTAT,TRNSDELT   BUILD DUMMY LIST RECORD FOR DELETE           
         MVI   LSTTRTYP,RECPST                                                  
         MVI   LSTPACT,C'*'                                                     
         MVC   LSTPACT+1(L'LSTPACT-1),LSTPACT                                   
         MVI   LSTPKOF,C'*'                                                     
         MVC   LSTPKOF+1(L'LSTPKOF-1),LSTPKOF                                   
         MVC   LSTPOFF,LSTPKOF                                                  
         MVI   LSTPCAC,C'*'                                                     
         MVC   LSTPCAC+1(L'LSTPCAC-1),LSTPCAC                                   
         ZAP   LSTPAMT,BCPZERO                                                  
         MVC   LSTPOTHR,BCSPACES                                                
         LH    RF,=Y(LC@DELD-TWAD)                                              
         LA    RF,TWAD(RF)                                                      
         MVC   LSTPOTHR(L'LC@DELD),0(RF)                                        
         B     LSTADD20                                                         
*                                                                               
LSTADD02 L     R4,AIO2                                                          
         USING TRNRECD,R4                                                       
         MVC   LSTPSEQ,ASKSEQN                                                  
LSTADD03 MVC   LSTTSTAT,TRNRSTAT                                                
         MVC   LSTTDA,IODA                                                      
         MVI   LSTTRTYP,RECPST                                                  
         MVC   LSTPACT,TRNKCULA                                                 
         MVC   LSTPKOF,TRNKOFF                                                  
         MVC   LSTPCAC,TRNKCULC                                                 
*                                                                               
         LA    RF,TRNRFST                                                       
         SR    R0,R0                                                            
LSTADD04 CLI   0(RF),0             TEST EOR                                     
         BE    LSTADD18                                                         
         CLI   0(RF),TRNELQ        TEST TRANSACTION ELEMENT                     
         BE    LSTADD14                                                         
         CLI   0(RF),TRSELQ        TEST TRANSACTION STATUS ELEMENT              
         BE    LSTADD12                                                         
*                                                                               
LSTADD06 IC    R0,1(RF)            BUMP TO NEXT ELEMENT                         
         AR    RF,R0                                                            
         B     LSTADD04                                                         
*                                                                               
         USING TRSELD,RF                                                        
LSTADD12 MVC   LSTPTRS1,TRSSTAT    EXTRACT TRANSACTION STATUS VALUES            
         MVC   LSTPTRS2,TRSSTAT2                                                
         MVC   LSTPTRS3,TRSSTAT3                                                
         B     LSTADD06                                                         
*                                                                               
         USING TRNELD,RF                                                        
LSTADD14 MVC   LSTPOFF,TRNOFFC     EXTRACT TRANSACTION VALUES                   
         MVC   LSTPSTAT,TRNSTAT                                                 
         ZAP   LSTPAMT,TRNAMNT                                                  
         CLI   PLNARRL,0           TEST NARRATIVE SET                           
         BNE   LSTADD06                                                         
         SR    R1,R1                                                            
         IC    R1,TRNLN                                                         
         SH    R1,=Y(TRNLN1Q+1)                                                 
         BNM   *+16                                                             
         MVI   PLNARRL,1                                                        
         MVI   PLNARR,C' '                                                      
         B     LSTADD06                                                         
         CLC   BCCPYEL+(CPYPROD-CPYELD)(L'CPYPROD),TRNKUNT                      
         BNE   LSTADD16                                                         
         CLC   LSTPOFF,=C'99'                                                   
         BNE   LSTADD16                                                         
         CLI   TRNLN,121                                                        
         BNE   LSTADD16                                                         
         LA    R1,14                                                            
LSTADD16 EX    R1,*+4                                                           
         MVC   PLNARR(0),TRNNARR                                                
         LA    R1,1(R1)                                                         
         STC   R1,PLNARRL                                                       
         B     LSTADD06                                                         
*                                                                               
LSTADD18 GOTO1 LSTOTHER            EXTRACT OTHER INFORMATION                    
*                                                                               
LSTADD20 GOTO1 ATSARIO,TSAADD      ADD LIST ENTRY TO TSAR                       
*                                                                               
LSTADD22 OC    CSPAG#LO,CSPAG#LO   SET LOW & HIGH RECORDS FOR PAGE              
         BNZ   *+10                                                             
         MVC   CSPAG#LO,LSTTRECN                                                
         MVC   CSPAG#HI,LSTTRECN                                                
*                                                                               
LSTADDY  CR    RB,RB               SET CC=EQUAL                                 
         B     EXIT                                                             
*                                                                               
LSTADDN  LTR   RB,RB               SET CC=NOT EQUAL FOR FULL PAGE               
         B     EXIT                                                             
         DROP  R2,RF                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD POSTING/LIST DISPLAY LINE                                     *         
***********************************************************************         
         SPACE 1                                                                
BLDLIN   NTR1  ,                                                                
         LR    R2,R1                                                            
         USING PLLINED,R2                                                       
         LR    RF,R2                                                            
         SH    RF,=Y(L'FVIHDR)                                                  
         NI    FVATRB-FVIHDR(RF),X'FF'-FVAHIGH                                  
         TM    LSTPIND1,LSTP1JEX                                                
         BZ    *+8                                                              
         OI    FVATRB-FVIHDR(RF),FVAHIGH                                        
         MVC   PLLACT,LSTPACT+(TRNKUNT-TRNKCULA)                                
         LA    R0,L'PLLCAC                                                      
         MVC   PLLCAC,LSTPCAC+(TRNKCUNT-TRNKCULC)                               
         CLI   PLLCAC,C' '                                                      
         BH    *+18                                                             
         MVC   PLLCAC(L'PLLCAC-1),PLLCAC+1  REMOVE LEADING SPACES               
         MVI   PLLCAC+(L'PLLCAC-1),C' '                                         
         BCT   R0,*-18                                                          
         MVC   PLLOFF,LSTPOFF                                                   
*&&UK*&& CURED LSTPAMT,(L'PLLAMT,PLLAMT),2,FLOAT=-                              
*&&US*&& CURED LSTPAMT,(L'PLLAMT,PLLAMT),2,MINUS=YES                            
         MVC   PLLDCR,BC@DR                                                     
         TM    LSTPSTAT,TRNSDR                                                  
         BNZ   *+10                                                             
         MVC   PLLDCR,BC@CR                                                     
         TM    LSTTSTAT,TRNSDELT   TEST TRANSACTION DELETED/NOT FOUND           
         BZ    *+10                                                             
         MVC   PLLDCR,BCSPACES     DR/CR STATUS UNKNOWN                         
         CLC   PLODIS,BCSPACES     DISPLAY OPTIONS SET?                         
         BNE   *+14                                                             
         MVC   PLLOTHR(L'PLLOTHR),LSTPOTHR  NO - SHOW POSTING DETAILS           
         B     BLDLINX                                                          
         GOTO1 LSTADIS                                                          
         MVC   PLLOTHR(L'PLLOTHR),BOWORK1                                       
BLDLINX  B     EXIT                                                             
         DROP  R2                                                               
         SPACE 1                                                                
PLLINED  DSECT                     ** POSTING LIST DISPLAY LINE **              
PLLACT   DS    CL(L'TRNKCULA-L'TRNKCPY)                                         
         DS    CL1                                                              
PLLCAC   DS    CL(L'TRNKCULC-L'TRNKCCPY)                                        
         DS    CL1                                                              
PLLOFF   DS    CL(L'TRNOFFC)                                                    
         DS    CL1                                                              
PLLAMT   DS    CL13                                                             
PLLDCR   DS    CL2                                                              
         DS    CL3                                                              
PLLOTHR  DS    CL23                                                             
BAT62    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTRACT KEY POSTING DETAILS (OTHER INFORMATION) FOR LIST SCREEN     *         
***********************************************************************         
         SPACE 1                                                                
LSTOTHER NTR1  ,                                                                
         LA    R5,BOWORK1                                                       
         MVC   BOWORK1,BCSPACES                                                 
         GOTO1 DISTIM,(R5)                                                      
         LR    R5,R1               POINT TO NEXT AVAILABLE AREA                 
*                                                                               
LSTOTH02 LA    R4,TRNRFST          SHOW FOREIGN CURRENCY DETAILS FIRST          
         ST    R4,BOADDR1                                                       
*&&UK                                                                           
         USING AFCELD,R4                                                        
LSTOTH04 CLI   AFCEL,0                                                          
         BE    LSTOTH10                                                         
         CLI   AFCEL,AFCELQ        FOREIGN CURRENCY DATA ELEMENT                
         BNE   LSTOTH07                                                         
*                                                                               
         USING AFCELD,R4                                                        
         GOTO1 VBLDCUR,BOPARM,AFCCURR,(X'00',BOWORK2),ACOM                      
         CLI   0(R1),0                                                          
         BNE   LSTOTH08                                                         
         CURED AFCAMNT,(15,(R5)),BOWORK2,MINUS=YES,ALIGN=LEFT,         X        
               CURSYMB=YES                                                      
         AR    R5,R0                                                            
         MVC   0(L'BCCOMMA,R5),BCCOMMA                                          
         LA    R5,1(R5)                                                         
         B     LSTOTH08                                                         
*                                                                               
         USING FFTELD,R4                                                        
LSTOTH07 CLI   FFTEL,FFTELQ        TEST FOR ASSOCIATED CURRENCY ELEMENT         
         BNE   LSTOTH08                                                         
         CLI   FFTTYPE,FFTTACUR                                                 
         BNE   LSTOTH08                                                         
         LH    RE,=Y(LC@CURRL-TWAD)                                             
         LA    RE,TWAD(RE)                                                      
         MVC   0(L'LC@CURRL,R5),0(RE)                                           
         LA    R5,L'LC@CURRL-1(R5)                                              
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVC   1(L'BCEQUAL,R5),BCEQUAL                                          
         MVC   1+L'BCEQUAL(L'AFCCURR,R5),FFTDATA                                
         MVC   1+L'BCEQUAL+L'AFCCURR(L'BCCOMMA,R5),BCCOMMA                      
         LA    R5,1+L'BCEQUAL+L'AFCCURR+L'BCCOMMA(R5)                           
*                                                                               
LSTOTH08 XR    RF,RF                                                            
         IC    RF,FFTLN                                                         
         AR    R4,RF                                                            
         B     LSTOTH04                                                         
*&&                                                                             
LSTOTH10 TM    LSTPIND1,LSTP1JEX  TEST SPLIT INVOICE JOB/EXP POSTING            
         BZ    LSTOTH11                                                         
         DROP  R4                                                               
         L     RE,AIO2                                                          
         USING TRNRECD,RE                                                       
         LH    RF,=Y(LC@REF-TWAD) REFERENCE NUMBER                              
         LA    RF,TWAD(RF)                                                      
         MVC   0(1,R5),0(RF)                                                    
         MVC   1(L'BCEQUAL,R5),BCEQUAL                                          
         MVC   L'BCEQUAL+1(L'TRNKREF,R5),TRNKREF                                
         LA    R5,1+L'BCEQUAL+L'TRNKREF-1(R5)                                   
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         LA    R5,1(R5)                                                         
         MVC   0(L'BCCOMMA,R5),BCCOMMA                                          
         LA    R5,1(R5)                                                         
         DROP  RE                                                               
                                                                                
LSTOTH11 L     R4,BOADDR1                                                       
LSTOTH12 CLI   0(R4),0                                                          
         BE    LSTOTH58                                                         
         CLI   0(R4),SCIELQ        SUBSIDIARY CASH INFO ELEMENT                 
         BE    LSTOTH30                                                         
         CLI   0(R4),CPJELQ        CLIENT/PRODUCT/JOB ELEMENT                   
         BE    LSTOTH16                                                         
         CLI   0(R4),SORELQ        SOURCE ACCOUNT ELEMENT                       
         BE    LSTOTH24                                                         
         CLI   0(R4),FFNELQ        FREE FORM/ORDER NUMBER ELEMENT               
         BE    LSTOTH14                                                         
         CLI   0(R4),TRNELQ        TRANSACTION ELEMENT                          
         BNE   LSTOTH56                                                         
*                                                                               
         USING TRNELD,R4                                                        
         TM    TRNSTAT,TRNSNOCM    NON-COMMISSIONABLE ITEM                      
         BZ    LSTOTH56                                                         
         LH    RF,=Y(UC$NCMSN-TWAD)                                             
         LA    RF,TWAD(RF)                                                      
         MVC   0(L'UC$NCMSN,R5),0(RF)                                           
         LA    R5,L'UC$NCMSN(R5)                                                
         B     LSTOTH52                                                         
*                                                                               
         USING FFNELD,R4                                                        
LSTOTH14 LH    RF,=Y(UC@ORDC-TWAD) ORDER NUMBER                                 
         LA    RF,TWAD(RF)                                                      
         MVC   0(1,R5),0(RF)                                                    
         MVC   1(L'BCEQUAL,R5),BCEQUAL                                          
         MVC   L'BCEQUAL+1(L'FFNONUM,R5),FFNONUM                                
         LA    R5,1+L'BCEQUAL+L'FFNONUM-1(R5)                                   
         B     LSTOTH52                                                         
*                                                                               
         USING CPJELD,R4                                                        
LSTOTH16 MVI   0(R5),C'S'          SOURCE ACCOUNT                               
         MVC   1(L'CPJTYPE,R5),CPJTYPE                                          
         MVC   L'CPJTYPE+1(L'BCEQUAL,R5),BCEQUAL                                
*                                                                               
         CLI   CPJTYPE,CPJTEXP     TEST EXPENSE SOURCE ACCOUNT                  
         BNE   LSTOTH18                                                         
         MVC   L'CPJTYPE+L'BCEQUAL+1(L'CPJEXP,R5),CPJEXP                        
         LA    R5,L'CPJTYPE+L'BCEQUAL+L'CPJEXP+1(R5)                            
         B     LSTOTH52                                                         
*                                                                               
LSTOTH18 CLI   CPJTYPE,CPJTJOB     TEST PRODUCTION SOURCE ACCOUNT               
         BNE   LSTOTH22                                                         
         LA    R0,3                MOVE OUT CLIENT/PRODUCT/JOB                  
         LA    R1,CPJCLI                                                        
         LA    RE,L'CPJTYPE+L'BCEQUAL+1(R5)                                     
LSTOTH20 MVC   0(L'CPJCLI,RE),0(R1)                                             
         LA    RE,L'CPJCLI-1(RE)                                                
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         BCT   RE,*-8                                                           
         LA    RE,2(RE)                                                         
         LA    R1,L'CPJCLI(R1)                                                  
         BCT   R0,LSTOTH20                                                      
         LR    R5,RE                                                            
         BCTR  R5,0                                                             
         B     LSTOTH54                                                         
*                                                                               
LSTOTH22 CLI   CPJTYPE,CPJTOTH     TEST OTHER SOURCE ACCOUNT                    
         BE    *+14                                                             
         MVC   0(L'CPJTYPE+L'BCEQUAL+1,R5),BCSPACES                             
         B     LSTOTH56                                                         
         MVI   1(R5),C'O'                                                       
         MVC   L'CPJTYPE+L'BCEQUAL+1(L'CPJOULA,R5),CPJOULA                      
         LA    R5,L'CPJTYPE+L'BCEQUAL+L'CPJOULA+1(R5)                           
         B     LSTOTH52                                                         
*                                                                               
         USING SORELD,R4                                                        
LSTOTH24 CLI   SORSYS,SORSACC      SOURCE - ACCOUNT SYSTEM                      
         BNE   LSTOTH28                                                         
         CLC   =C'SJ',SORAUNT      PRODUCTION                                   
         BE    LSTOTH26                                                         
         CLC   =C'SE',SORAUNT      EXPENSE                                      
         BE    LSTOTH26                                                         
         MVC   0(2,R5),=C'SO'      OTHER                                        
         MVC   2(L'BCEQUAL,R5),BCEQUAL                                          
         MVC   2+L'BCEQUAL(L'SORAULA,R5),SORAULA                                
         LA    R5,2+L'BCEQUAL+L'SORAULA-1(R5)                                   
         B     LSTOTH52                                                         
LSTOTH26 MVC   0(L'SORAUNT+L'SORALDG,R5),SORAUNT                                
         MVC   L'SORAUNT+L'SORALDG(L'BCEQUAL,R5),BCEQUAL                        
         MVC   L'SORAUNT+L'SORALDG+L'BCEQUAL(L'SORAACT,R5),SORAACT              
         LA    R5,L'SORAUNT+L'SORALDG+L'BCEQUAL+L'SORAACT-1(R5)                 
         B     LSTOTH52                                                         
*                                                                               
LSTOTH28 CLI   SORSYS,SORSMED      SOURCE - MEDIA SYSTEM                        
         BNE   LSTOTH56                                                         
         MVC   BOWORK2,BCSPACES                                                 
         MVC   BOWORK2(L'SORMCLI),SORMCLI                                       
         MVC   BOWORK2+L'SORMCLI+1(L'SORMPRO),SORMPRO                           
         MVC   BOWORK2+L'SORMCLI+L'SORMPRO+2(L'SORMCAM),SORMCAM                 
         LA    R2,L'SORMCLI+L'SORMPRO+L'SORMCAM+2                               
         GOTO1 VSQUASH,BOPARM,BOWORK2,(C'/',(R2))                               
         MVC   0(L'SORAUNT+L'SORALDG,R5),=C'SM'                                 
         LA    R5,L'SORAUNT+L'SORALDG(R5)                                       
         MVC   0(L'SORMMED,R5),SORMMED                                          
         MVC   1(L'BCEQUAL,R5),BCEQUAL                                          
         L     RE,4(R1)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+4                                                              
         MVC   L'BCEQUAL+1(0,R5),BOWORK2                                        
         LA    R5,2(RE,R5)                                                      
         B     LSTOTH52                                                         
*                                                                               
         USING SCIELD,R4                                                        
LSTOTH30 CLI   SCITYPE,C'A'                                                     
         BL    LSTOTH56                                                         
         CLI   SCITYPE,C'Z'                                                     
         BNH   *+20                                                             
         CLI   SCITYPE,C'0'                                                     
         BL    LSTOTH56                                                         
         CLI   SCITYPE,C'9'                                                     
         BH    LSTOTH56                                                         
*&&UK                                                                           
         CLI   SCITYPE,SCITNOCD                                                 
         BE    LSTOTH56            SKIP IF TYPE = X                             
         CLI   SCITYPE,SCITMILE                                                 
         BNE   LSTOTH32                                                         
         CLC   =C'SI',LSTPACT+1                                                 
         BE    LSTOTH32                                                         
         SRP   SCIAMNT,2,0                                                      
*&&                                                                             
LSTOTH32 MVC   0(L'SCITYPE,R5),SCITYPE                                          
         MVC   L'SCITYPE(L'BCEQUAL,R5),BCEQUAL                                  
*                                                                               
*&&US*&& CURED SCIAMNT,(12,2(R5)),2,MINUS=YES,ALIGN=LEFT                        
*&&UK*&& CURED SCIAMNT,(12,2(R5)),2,FLOAT=-,ALIGN=LEFT                          
*                                                                               
LSTOTH40 LA    R5,L'SCITYPE+L'BCEQUAL(R5)                                       
         AR    R5,R0                                                            
         XR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         AR    RF,R4                                                            
         CLI   0(RF),PCIELQ        PROJECT CONTROL ELEMENT                      
         BNE   LSTOTH54                                                         
         USING PCIELD,RF                                                        
         CLI   PCILN,PCILN2Q                                                    
         BL    LSTOTH54                                                         
         MVC   0(L'BCSLASH,R5),BCSLASH                                          
         MVC   L'BCSLASH((L'PCIPRJT-(TRNKACT-TRNKCULA)),R5),PCIPRJT+(TRX        
               NKACT-TRNKCULA)                                                  
         LA    R5,L'BCSLASH+L'PCIPRJT-(TRNKACT-TRNKCULA)(R5)                    
         CLI   0(R5),C' '                                                       
         BNE   *+8                                                              
         BCT   R5,*-8                                                           
         MVC   1(L'BCSLASH,R5),BCSLASH                                          
         MVC   L'BCSLASH+1(L'PCITSK,R5),PCITSK                                  
         LA    R5,L'BCSLASH+L'PCITSK+1(R5)                                      
*                                                                               
LSTOTH52 CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         LA    R5,1(R5)                                                         
*                                                                               
LSTOTH54 MVC   0(L'BCCOMMA,R5),BCCOMMA                                          
         LA    R5,1(R5)                                                         
*                                                                               
LSTOTH56 XR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     LSTOTH12                                                         
*                                                                               
LSTOTH58 CLC   BOWORK1,BCSPACES                                                 
         BE    LSTOTHX                                                          
*                                                                               
         CLC   BOWORK1+L'PLLOTHR(L'BCCOMMA),BCCOMMA                             
         BE    LSTOTHX                                                          
         CLI   BOWORK1+L'PLLOTHR,C' '                                           
         BNE   *+14                                                             
         BCTR  R5,0                                                             
         MVI   0(R5),C' '          REMOVE LAST COMMA                            
         B     LSTOTHX                                                          
*                                                                               
         LA    R1,L'PLLOTHR                                                     
         LA    R5,BOWORK1+L'PLLOTHR-1                                           
LSTOTH60 CLC   0(L'BCCOMMA,R5),BCCOMMA                                          
         MVI   0(R5),C' '                                                       
         BE    LSTOTHX                                                          
         BCTR  R5,0                                                             
         BCT   R1,LSTOTH60                                                      
*                                                                               
LSTOTHX  MVC   LSTPOTHR(L'PLLOTHR),BOWORK1                                      
         B     EXIT                                                             
         DROP  R4,RF                                                            
         EJECT                                                                  
***********************************************************************         
* SET VARIABLE COLUMN HEADINGS                                        *         
***********************************************************************         
         SPACE 1                                                                
LSTVHED  ST    RE,BOADDR1                                                       
         TWAXC PLIOTHTH,PLIACTTH-1,PROT=Y                                       
         TWAXC PLIOTHUH,PLIACT1H-1,PROT=Y                                       
         CLC   PLODIS,BCSPACES                                                  
         BNE   LSTVHED2                                                         
         LH    RF,=Y(LC@OTHRI-TWAD)                                             
         LA    RF,TWAD(RF)                                                      
         MVC   PLIOTHT(L'LC@OTHRI),0(RF)  OTHER INFORMATION                     
         BAS   RE,LSTVHU                                                        
         B     LSTVHEDX                                                         
LSTVHED2 LA    R1,PLODIS                                                        
         LH    RF,DISNAME-DISTABD(R1)                                           
         LA    RF,TWAD(RF)                                                      
         MVC   PLIOTHT(L'LC@OTHRI),0(RF)   VALID ACTIONS                        
         BAS   RE,LSTVHU                                                        
LSTVHEDX L     RE,BOADDR1                                                       
         BR    RE                                                               
*                                                                               
LSTVHU   ST    RE,BOADDR2          UNDERLINE HEADINGS                           
         LA    RE,PLIOTHT                                                       
         LA    RF,L'PLIOTHT-1(RE)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,RE               RF=EXECUTE L'HEADING                         
         BM    LSTVHUX             NOTHING TO UNDERLINE                         
         LA    RE,PLIOTHU                                                       
         MVI   0(RE),C'-'          UNDERLINE FIRST CHARACTER                    
         SH    RF,=H'1'            DROP A CHARACTER FOR SECOND MOVE             
         BM    LSTVHUX             SINGLE CHARACTER HEADING                     
         EX    RF,*+4                                                           
         MVC   1(0,RE),0(RE)       UNDERLINE REST OF FIRST HEADING              
LSTVHUX  L     RE,BOADDR2                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* EXTRACT ALTERNATIVE DISPLAY DATA FOR LIST SCREEN                    *         
***********************************************************************         
         SPACE 1                                                                
LSTADIS  NTR1                                                                   
         LA    R5,BOWORK1                                                       
         MVC   BOWORK1,BCSPACES                                                 
         CLC   PLODIS+1(L'DISNAME),=AL2(LC@VALAC-TWAD)                          
         BE    LSTADIS0            'VALID ACTIONS' DATA WANTED                  
         MVC   BOWORK1(L'LSTTDA),CSLSTCUR+(LSTTDA-LSTTABD)                      
         XOUT  BOWORK1,BOWORK1+4,4                                              
         MVC   BOWORK1(4),=C'D/A='  ELSE DISPLAY DISK ADDRESS                   
         B     LSTADISX                                                         
*                                                                               
LSTADIS0 L     R1,AOVERSEL                                                      
         USING SELTABD,R1                                                       
LSTADIS2 CLI   SELTABD,EOT                                                      
         BE    LSTADIS6                                                         
         STM   RE,R1,BOPARM                                                     
         GOTO1 ATSTMIX,SELTPARM    VALIDATE RECORD/ACTION                       
         LM    RE,R1,BOPARM                                                     
         BNE   LSTADIS4                                                         
         MVC   BCHALF,SELTMASK                                                  
         NC    BCHALF,LSTTMASK                                                  
         CLC   BCHALF,SELTMASK                                                  
         BNE   LSTADIS4                                                         
         XR    RE,RE                                                            
         ICM   RE,3,SELTDSPM       MIXED CASE ACTION WORD                       
         LA    RE,TWAD(RE)                                                      
         MVC   0(PLVACTWD,R5),0(RE)                                             
         MVC   PLVACTWD(L'BCCOMMA,R5),BCCOMMA                                   
         LA    R5,L'BCCOMMA+PLVACTWD(R5)                                        
LSTADIS4 LA    R1,SELTABL(R1)                                                   
         B     LSTADIS2                                                         
*                                                                               
LSTADIS6 CLC   BOWORK1,BCSPACES                                                 
         BE    LSTADISX                                                         
*                                                                               
         CLI   BOWORK1+L'PLLOTHR,C' '                                           
         BNE   *+14                                                             
         BCTR  R5,0                                                             
         MVI   0(R5),C' '                                                       
         B     LSTADISX                                                         
*                                                                               
         LA    R5,BOWORK1+L'PLLOTHR-1                                           
         CLC   0(L'BCCOMMA,R5),BCCOMMA                                          
         MVI   0(R5),C' '                                                       
         BE    LSTADISX                                                         
         BCT   R5,*-14                                                          
*                                                                               
LSTADISX B     EXIT                                                             
         DROP  R1,R3,RC                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY TYPE OF TIME FOR US COSTING SYSTEM                          *         
*                                                                     *         
* NTRY - R1=A(OUTPUT AREA)                                            *         
* EXIT - R1=A(NEXT AVAILABLE OUTPUT AREA)                             *         
***********************************************************************         
         SPACE 1                                                                
DISTIM   TM    CSLSTCUR+(LSTPTRS2-LSTTABD),TRSSTADJ+TRSSTMSS+TRSSTIME           
         BZ    DISTIMX                                                          
         LH    RF,=Y(LC@TYPE-TWAD) FORMAT TYPE OF TIME                          
         LA    RF,TWAD(RF)                                                      
         MVC   0(L'LC@TYPE,R1),0(RF)                                            
         LA    R1,L'LC@TYPE-1(R1)                                               
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   1(L'BCEQUAL,R1),BCEQUAL                                          
         LH    RF,=Y(LC@TIME-TWAD)                                              
         TM    CSLSTCUR+(LSTPTRS2-LSTTABD),TRSSTIME                             
         BNZ   DISTIM02                                                         
         LH    RF,=Y(LC@MSNG-TWAD)                                              
         TM    CSLSTCUR+(LSTPTRS2-LSTTABD),TRSSTMSS                             
         BNZ   DISTIM02                                                         
         LH    RF,=Y(LC@ADJ-TWAD)                                               
DISTIM02 LA    RF,TWAD(RF)                                                      
         MVC   2(L'LC@TIME,R1),0(RF)                                            
         LA    R1,1+L'LC@TIME(R1)                                               
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   1(L'BCCOMMA,R1),BCCOMMA                                          
         LA    R1,1+L'BCCOMMA(R1)                                               
DISTIMX  BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* AUTHORISE AND IMPLEMENT NARRATIVE CHANGES ON POSTING/LIST SCREEN    *         
***********************************************************************         
         SPACE 1                                                                
PSTNAR   NTR1  WORK=(RC,PRORATX-PRORATD)                                        
         USING PRORATD,RC                                                       
         TM    PLNARFLG,PLNARINP   TEST WRITING BACK NARRATIVE                  
         BZ    PSTNAR02                                                         
         LA    R0,PLNLINM                                                       
         LA    R2,PLINAR1H                                                      
         TM    FVIIND-FVIHDR(R2),FVIVAL                                         
         BZ    PSTNAR01            VALIDATION REQUIRED                          
         LA    R2,L'PLINAR1+L'PLINAR1H(R2)                                      
         BCT   R0,*-12                                                          
         B     PSTNARX                                                          
*                                                                               
PSTNAR01 LA    R0,PLNLINM                                                       
         LA    R2,PLINAR1H                                                      
         OI    FVIIND-FVIHDR(R2),FVIVAL                                         
         LA    R2,L'PLINAR1+L'PLINAR1H(R2)                                      
         BCT   R0,*-8                                                           
*                                                                               
PSTNAR02 LA    RF,BCITECUR         READ BATCH ITEM RECORD                       
         MVC   IODAOVER,LSTTDA-LSTTABD(RF)                                      
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO1             R2=A(BATCH ITEM RCORD)                       
         USING TBARECD,R2                                                       
         OI    PLNARFLG,PLNARCHG   DEFAULT ALLOWS CHANGE OF NARRATIVE           
         TM    BCITECUR+(LSTIIND1-LSTTABD),LSTI1SIN TEST FOR SPLIT INV.         
         BO    PSTNAR03                                                         
         TM    CSBIND3,TYPIANAR    TEST TYPE ADDS AUTOMATIC NARRATIVE           
         BO    PSTNAR03                                                         
         CLC   TBAKBCHR,BCSPACES   TEST PERSON ABSENT (NON-INPUT BATCH)         
         BH    *+8                                                              
PSTNAR03 NI    PLNARFLG,FF-PLNARCHG                                             
*                                                                               
         LA    R2,TBARFST                                                       
         USING ASKELD,R2                                                        
         SR    R0,R0                                                            
*                                                                               
PSTNAR04 CLI   ASKEL,0             TEST EOR                                     
         BE    PSTNARX                                                          
         CLI   ASKEL,ASKELQ        SEARCH FOR TRANSACTION RECORD KEYS           
         BNE   PSTNAR20                                                         
*                                                                               
         MVC   IOKEY,ASKKEY                                                     
         GOTO1 AIO,IORDUP+IOACCMST+IO2                                          
         BE    PSTNAR05                                                         
         BH    *+6                                                              
         DC    H'0'                                                             
         NI    PLNARFLG,FF-PLNARCHG                                             
         B     PSTNARX                                                          
PSTNAR05 L     R3,AIO2             R3=A(TRANSACTION RECORD)                     
         USING TRNRECD,R3                                                       
         LA    R4,TRNRFST                                                       
         TM    PLNARFLG,PLNARINP   WRITING BACK NEW INPUT?                      
         BZ    PSTNAR08                                                         
*                                                                               
         TM    TRNKSTAT,TRNSARCH   TRANSACTION ON ARCHIVE FILE                  
         BZ    *+12                                                             
         NI    PLNARFLG,FF-PLNARCHG                                             
         B     PSTNARX                                                          
*                                                                               
         USING TRNELD,R4                                                        
         CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                TRNEL MUST BE FIRST ELEMENT                  
*                                                                               
         TM    PLNARFLG,PLNARNEW   HAS NEW NARRATIVE BEEN VALIDATED?            
         BO    PSTNAR06                                                         
         GOTO1 AVALNAR,BOPARM,('PLNLINM',PLINAR1H),0                            
         TM    PLNARFLG,PLNARSHT                                                
         BZ    *+12                                                             
         LA    RE,PDPRDNLN         SHORT LENGTH NARRATIVE FOR W/C 99            
         B     *+10                                                             
         XR    RE,RE                                                            
         ICM   RE,3,BOHALF1                                                     
         STC   RE,PLNARRL                                                       
         LA    RF,L'PLNARR         MAXIMUM LENGTH OF NARRATIVE                  
         CR    RF,RE                                                            
         BNL   *+6                                                              
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   PLNARR(0),BOELEM                                                 
         OI    PLNARFLG,PLNARNEW                                                
*                                                                               
PSTNAR06 MVI   BOELEM,C' '         BUILD REVISED TRNEL HERE                     
         MVC   BOELEM+1(L'BOELEM-1),BOELEM                                      
*                                                                               
         XR    RE,RE                                                            
         IC    RE,PLNARRL                                                       
         MVC   BOELEM(TRNLN1Q),0(R4)                                            
         LA    RF,TRNLN1Q(RE)                                                   
         STC   RF,BOELEM+1         NEW ELEMENT LENGTH                           
         CLC   BCCPYEL+(CPYPROD-CPYELD)(L'CPYPROD),TRNKUNT                      
         BNE   PSTNAR07                                                         
         CLC   TRNANAL,=C'99'                                                   
         BNE   PSTNAR07                                                         
         CLI   TRNLN,121                                                        
         BNE   PSTNAR07                                                         
         XR    RF,RF                                                            
         IC    RF,TRNLN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   BOELEM(0),0(R4)                                                  
*                                                                               
PSTNAR07 BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   BOELEM+TRNLN1Q(0),PLNARR                                         
         MVI   BOELEM,X'01'        TRNEL MUST BE FIRST ELEMENT                  
*                                                                               
         XR    RE,RE                                                            
         IC    RE,TRNLN                                                         
         XR    R0,R0                                                            
         ICM   R0,3,TRNRLEN                                                     
         SR    R0,RE               SUBTRACT OLD L'ELEMENT FROM KEY              
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('TRNELQ',TRNRECD),0,0               
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,BOELEM                                                        
         XR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R0,R1                                                            
         STCM  R0,3,TRNRLEN        ADD NEW L'ELEMENT TO KEY                     
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),TRNRECD,TRNELD,0,0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,TRNRFST                                                       
         CLI   TRNEL,X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRNEL,TRNELQ        FIRST ELEMENT IS TRANSACTION ELEMENT         
*                                                                               
         GOTO1 AIO,IOWRITE+IOACCMST+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PSTNAR20                                                         
*                                                                               
         USING TRNELD,R4                                                        
PSTNAR08 CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                TRNEL MUST BE FIRST ELEMENT                  
         CLC   BCCPYEL+(CPYPROD-CPYELD)(L'CPYPROD),TRNKUNT                      
         BNE   PSTNAR10                                                         
         CLC   TRNANAL,=C'99'      TEST OLD BILLING STYLE POSTING               
         BNE   PSTNAR09                                                         
         CLI   TRNLN,121                                                        
         BNE   PSTNAR09                                                         
         OI    PLNARFLG,PLNARSHT   SHORT NARRATIVE FOR W/C 99                   
*                                                                               
PSTNAR09 GOTO1 VPRORATA,BOPARM,TRNRECD,0,ACOM,0,PRORATD,0                       
         CP    PM$ANVBL,PA$NET                                                  
         BE    *+8                                                              
         NI    PLNARFLG,FF-PLNARCHG BILL TXS = FIXED NARRATIVE                  
*                                                                               
PSTNAR10 LA    RF,NAUTHTAB         CHECK CHANGE PERMITTED                       
PSTNAR11 CLI   0(RF),EOT                                                        
         BE    PSTNAR19            U/L DOES NOT PREVENT CHANGE                  
         CLC   0(2,RF),TRNKUNT                                                  
         BE    PSTNAR12            POSSIBLE PREVENTION                          
         LA    RF,NAUTHQ(RF)                                                    
         B     PSTNAR11                                                         
*                                                                               
PSTNAR12 CLI   0(R4),0                                                          
         BE    PSTNAR19            TRANSACTION DOES NOT PREVENT CHANGE          
         CLC   0(1,R4),2(RF)                                                    
         BE    PSTNAR16                                                         
PSTNAR14 IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     PSTNAR12                                                         
*                                                                               
         USING TRSELD,R4                                                        
PSTNAR16 CLI   2(RF),TRSELQ                                                     
         BE    *+6                                                              
         DC    H'0'                TABLE DATA INCOMPATIBLE                      
         OC    TRSUDAT,TRSUDAT     TEST USED                                    
         BNZ   *+12                                                             
         TM    TRSSTAT,TRSSACHQ    TEST PAID BY SYSTEM CHEQUE                   
         BZ    PSTNAR14                                                         
         NI    PLNARFLG,FF-PLNARCHG PAID TXS = FIXED NARRATIVE                  
*                                                                               
PSTNAR19 TM    PLNARFLG,PLNARCHG   TEST NARRATIVE CHANGE ALLOWED                
         BO    PSTNAR20                                                         
         LA    R0,PLNLINM          PROTECT NARRATIVE FIELDS                     
         LA    R2,PLINAR1H                                                      
         OI    FVATRB-FVIHDR(R2),FVAPROT                                        
         LA    R2,L'PLINAR1+L'PLINAR1H(R2)                                      
         BCT   R0,*-8                                                           
         B     PSTNARX                                                          
*                                                                               
PSTNAR20 XR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     PSTNAR04                                                         
*                                                                               
PSTNARX  NI    PLNARFLG,FF-(PLNARFST+PLNARNEW)                                  
         B     EXIT                                                             
         DROP  R2,R3,R4,RC                                                      
         EJECT                                                                  
***********************************************************************         
* DISPLAY NARRATIVE ON POSTING/LIST SCREEN                            *         
***********************************************************************         
         SPACE 1                                                                
PSTDIN   ST    RE,BOADDR1                                                       
         CLI   PLNARRL,0           ANY NARRATIVE?                               
         BNE   PSTDIN01                                                         
         TM    PLNARFLG,PLNARCHG   YES - PROTECT IF CHANGE PREVENTED            
         BO    PSTDINX                                                          
         LA    R0,PLNLINM                                                       
         LA    R2,PLINAR1H                                                      
         OI    FVATRB-FVIHDR(R2),FVAPROT                                        
         LA    R2,L'PLINAR1+L'PLINAR1H(R2)                                      
         BCT   R0,*-8                                                           
         B     PSTDINX                                                          
*                                                                               
PSTDIN01 LA    R0,PLNLINM          MAXIMUM NUMBER OF LINES                      
         GOTO1 VCHOPPER,BOPARM,(PLNARRL,PLNARR),('PLNLINL',BOELEM),(R0)         
         ICM   R1,15,8(R1)         ACTUAL NUMBER OF LINES                       
         BZ    PSTDIN04                                                         
         CR    R1,R0               DON'T TRY TO SHOW MORE THAN MAXIMUM          
         BH    PSTDIN04                                                         
*                                                                               
         LA    R0,PLNLINM                                                       
         LA    R2,PLINAR1H                                                      
         LA    RF,BOELEM                                                        
PSTDIN02 MVC   L'PLINAR1H(PLNLINL,R2),0(RF)                                     
         OI    FVOIND-FVIHDR(R2),FVOXMT                                         
         TM    PLNARFLG,PLNARCHG   PROTECT IF CHANGE PREVENTED                  
         BO    *+8                                                              
         OI    FVATRB-FVIHDR(R2),FVAPROT                                        
         LA    R2,L'PLINAR1+L'PLINAR1H(R2)                                      
         LA    RF,PLNLINL(RF)                                                   
         BCT   R0,PSTDIN02                                                      
                                                                                
PSTDIN04 LA    R0,PLNLINM          SET VALIDATED BITS ON                        
         LA    R1,PLINAR1H                                                      
         OI    FVIIND-FVIHDR(R1),FVIVAL                                         
         AHI   R1,L'PLINAR1+L'PLINAR1H                                          
         BCT   R0,*-8                                                           
*                                                                               
PSTDINX  L     RE,BOADDR1                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE POSTING OPTIONS                                            *         
***********************************************************************         
         SPACE 1                                                                
         DROP  R6,R7,R8,RB                                                      
PLOVAL   NMOD1 250,**PLOV**,CLEAR=YES                                           
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         B     PLOVDIS                                                          
PLOVALX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE DISPLAY=?                                                  *         
***********************************************************************         
         SPACE 1                                                                
PLOVDIS  MVC   BCWORK,BCSPACES                                                  
         LA    R1,FVIFLD                                                        
         XR    R0,R0                                                            
         ICM   R0,1,FVXLEN         ONLY ONE CHARACTER INPUT ACCEPTED            
         BNZ   PLOVDI04                                                         
         LA    RE,DISTAB                                                        
         USING DISTABD,RE                                                       
PLOVDI02 CLI   DISTABD,EOT                                                      
         BE    PLOVDI04                                                         
         CLC   DISCHR,0(R1)                                                     
         BE    *+12                                                             
         LA    RE,DISTABL(RE)                                                   
         B     PLOVDI02                                                         
         TM    DISIND,DISIDDSQ     TEST DDS-ONLY OPTION                         
         BZ    *+12                NO - OK                                      
         TM    CUSTAT,CUSDDS       TEST DDS TERMINAL                            
         BZ    PLOVDI04            NO - ERROR                                   
         MVC   BCWORK(DISTABL),DISCHR                                           
         B     PLOVDISX                                                         
PLOVDI04 MVC   FVMSGNO,=AL2(AE$INCOL)                                           
         MVC   FVXTRA(1),0(R1)                                                  
PLOVDISX B     PLOVALX                                                          
         SPACE 1                                                                
DISTAB   DS    0X                                                               
         SPACE 1                                                                
         DC    C'?',AL2(LC@VALAC-TWAD),AL1(0)                                   
         DC    C'@',AL2(LC@OTHRI-TWAD),AL1(DISIDDSQ)                            
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
DISTABD  DSECT                                                                  
DISCHR   DS    XL1                 COLUMN CHARCTER                              
DISNAME  DS    AL2                 DISPLACEMENT TO COLUMN NAME                  
DISIND   DS    XL1                 INDICATOR BYTE                               
DISIDDSQ EQU   X'80'               DDS-ONLY OPTION                              
DISTABL  EQU   *-DISTABD                                                        
BAT62    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* EXITS FROM PROGRAM                                                  *         
***********************************************************************         
         SPACE 1                                                                
TSTMSG   CLC   FVMSGNO,=AL2(FVFSET)                                             
         BNE   EXIT                                                             
         SPACE 1                                                                
SETCUR   LA    R1,BASOLY1H         SET CURSOR TO FIRST UNPROT FIELD             
         SR    RE,RE                                                            
         LA    RF,OSVALS-1                                                      
SETCUR02 ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BZ    *+10                                                             
         BXLE  R1,RE,SETCUR02                                                   
         DC    H'0'                CAN'T FIND AN INPUT FIELD                    
         STCM  R1,15,FVADDR                                                     
         B     EXIT                                                             
         SPACE 1                                                                
ERRINVAS MVC   FVMSGNO,=AL2(AE$INACS)                                           
         LA    R0,BASACTH                                                       
         ST    R0,FVADDR                                                        
         SPACE 1                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
FF       EQU   X'FF'                                                            
         LTORG                                                                  
ACCMST   DC    C'ACCMST '                                                       
DMCB     EQU   BOPARM                                                           
         EJECT                                                                  
***********************************************************************         
* POSTING LIST OPTION TABLE                                           *         
***********************************************************************         
         SPACE 1                                                                
PLOTAB   DS    0X                                                               
         SPACE 1                                                                
         DC    AL2(UC8DSP-TWAD,UC3DSP-TWAD)                                     
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,0,1,L'PLODIS,L'PLODIS)                               
         DC    AL1(1)                                                           
         DC    AL2(1,PLODIS-PLVALS)                                             
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
         SPACE 1                                                                
PLOTABX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* NARRATIVE AUTHORISATION TABLE                                       *         
***********************************************************************         
         SPACE 1                                                                
NAUTHTAB DS    0X                                                               
         DC    C'SF',AL1(TRSELQ)                                                
NAUTHQ   EQU   *-NAUTHTAB                                                       
         DC    C'ST',AL1(TRSELQ)                                                
         DC    C'SV',AL1(TRSELQ)                                                
         DC    C'SX',AL1(TRSELQ)                                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* VENDOR LEDGERS LIST                                                 *         
***********************************************************************         
         SPACE 1                                                                
VENDLIST DS    0X                                                               
*&&UK*&& DC    C'SF',C'ST',C'SV',C'SX'                                          
*&&US*&& DC    C'SP',C'SQ',C'SS',C'ST',C'SU',C'SV',C'SW',C'SX',C'SY'            
         DC    AL1(EOT)                                                         
*                                                                               
BANKLIST DS    0X                                                               
         DC    C'SC'                                                            
UNTLDGL  EQU   *-BANKLIST                                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DATE DISPLAY TABLE                                                  *         
***********************************************************************         
         SPACE 1                                                                
PDDTAB   DS    0X                                                               
         DC    AL2(PDDRAFT-PDWORKD),AL1(PDDLD2),AL1(PDDC2,PDDLG)                
         DC    AL1(L'LC$DRTON),AL2(LC$DRTON-TWAD),AL1(0)                        
         SPACE 1                                                                
         DC    AL2(PDEFF-PDWORKD),AL1(PDDLD2),AL1(PDDC2,PDDLG)                  
         DC    AL1(L'LC$EFFON),AL2(LC$EFFON-TWAD),AL1(0)                        
         SPACE 1                                                                
         DC    AL2(PDLIVE-PDWORKD),AL1(PDDLD2),AL1(PDDC2,PDDLG)                 
         DC    AL1(L'LC$LIVON),AL2(LC$LIVON-TWAD),AL1(0)                        
         SPACE 1                                                                
         DC    AL2(PDDUE-PDWORKD),AL1(PDDLD2),AL1(PDDC2,PDDLG)                  
         DC    AL1(L'LC$DUEON),AL2(LC$DUEON-TWAD),AL1(0)                        
         SPACE 1                                                                
         DC    AL2(PDPOSTMA-PDWORKD),AL1(PDDLD2),AL1(PDDP2,PDDSH)               
         DC    AL1(L'LC$PSTGM),AL2(LC$PSTGM-TWAD),AL1(0)                        
         SPACE 1                                                                
         DC    AL2(PDREVD-PDWORKD),AL1(PDDLD2),AL1(PDDC2,PDDLG)                 
         DC    AL1(L'LC$RVRON),AL2(LC$RVRON-TWAD),AL1(0)                        
         SPACE 1                                                                
         DC    AL2(PDREVDMA-PDWORKD),AL1(PDDLD2),AL1(PDDP2,PDDSH)               
         DC    AL1(L'LC$RVRMA),AL2(LC$RVRMA-TWAD),AL1(0)                        
         SPACE 1                                                                
         DC    AL2(PDUSED-PDWORKD),AL1(PDDLD2),AL1(PDDC2,PDDLG)                 
         DC    AL1(L'LC$MAUON),AL2(LC$MAUON-TWAD),AL1(PDDSSUU)                  
         SPACE 1                                                                
         DC    AL2(PDUSED-PDWORKD),AL1(PDDLD2),AL1(PDDC2,PDDLG)                 
         DC    AL1(L'LC$BLDON),AL2(LC$BLDON-TWAD),AL1(PDDSSUB)                  
         SPACE 1                                                                
         DC    AL2(PDUSED-PDWORKD),AL1(PDDLD2),AL1(PDDC2,PDDLG)                 
         DC    AL1(L'LC$PAION),AL2(LC$PAION-TWAD),AL1(PDDSSUP)                  
         SPACE 1                                                                
         DC    AL2(PDUSED-PDWORKD),AL1(PDDLD2),AL1(PDDC2,PDDLG)                 
         DC    AL1(L'LC$CTRON),AL2(LC$CTRON-TWAD),AL1(PDDSSUC)                  
         SPACE 1                                                                
         DC    AL2(PDUSEDMA-PDWORKD),AL1(PDDLD2),AL1(PDDP2,PDDSH)               
         DC    AL1(L'LC$MAUMA),AL2(LC$MAUMA-TWAD),AL1(PDDSSUU)                  
         SPACE 1                                                                
         DC    AL2(PDUSEDMA-PDWORKD),AL1(PDDLD2),AL1(PDDP2,PDDSH)               
         DC    AL1(L'LC$BLGMA),AL2(LC$BLGMA-TWAD),AL1(PDDSSUB)                  
         SPACE 1                                                                
         DC    AL2(PDUSEDMA-PDWORKD),AL1(PDDLD2),AL1(PDDP2,PDDSH)               
         DC    AL1(L'LC$PAIMA),AL2(LC$PAIMA-TWAD),AL1(PDDSSUP)                  
         SPACE 1                                                                
         DC    AL2(PDUSEDMA-PDWORKD),AL1(PDDLD2),AL1(PDDP2,PDDSH)               
         DC    AL1(L'LC$CTRMA),AL2(LC$CTRMA-TWAD),AL1(PDDSSUC)                  
         SPACE 1                                                                
         DC    AL2(PDUPDAT-PDWORKD),AL1(PDDLD2),AL1(PDDC2,PDDLG)                
         DC    AL1(L'LC$UPGON),AL2(LC$UPGON-TWAD),AL1(0)                        
         SPACE 1                                                                
         DC    AL2(PDPEEL-PDWORKD),AL1(PDDLD2),AL1(PDDC2,PDDLG)                 
         DC    AL1(L'LC$PELON),AL2(LC$PELON-TWAD),AL1(0)                        
         SPACE 1                                                                
         DC    AL2(PDOLACT-PDWORKD),AL1(PDDLD3),AL1(PDDP3,PDDLG)                
         DC    AL1(L'LC$LSTON),AL2(LC$LSTON-TWAD),AL1(0)                        
         SPACE 1                                                                
PDDTABX  DC    AL2(EOT)                                                         
         SPACE 1                                                                
PDDTABD  DSECT                     ** DATE DISPLAY TABLE **                     
PDDLST   DS    AL2                 DISPLACEMENT TO DATE IN W/S                  
PDDLDT   DS    XL1                 LENGTH OF DATE                               
PDDLD2   EQU   2                   TWO BYTE                                     
PDDLD3   EQU   3                   THREE BYTE                                   
PDDCIN   DS    XL1                 DATCON INPUT TYPE                            
PDDP2    EQU   1                   INPUT  - PACKED 2 BYTE                       
PDDP3    EQU   1                          - PACKED 3 BYTE                       
PDDC2    EQU   2                          - COMPRESSED 2 BYTE                   
PDDOL    EQU   X'80'                      - RETURN OUTPUT LENGTH                
PDDCOUT  DS    XL1                 DATCON OUTPUT TYPE                           
PDDLG    EQU   17                  OUTPUT - DDMMMYY                             
PDDSH    EQU   18                         - MMMYY                               
PDDLPREF DS    AL1                 L'PREFIX COMPONENT (MAX 15 CHRS)             
PDDPREF  DS    AL2                 DISPLACEMENT TO PREFIX                       
PDDSUBS  DS    XL1                 SUB-STATUS INDICATOR                         
PDDSSUU  EQU   1                   USED                                         
PDDSSUB  EQU   2                   USED - BILLED                                
PDDSSUP  EQU   3                   USED - PAID                                  
PDDSSUC  EQU   4                   USED - CONTRA'D                              
PDDTABL  EQU   *-PDDTABD                                                        
BAT62    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* TRANSACTION ANALYSIS TABLE                                          *         
***********************************************************************         
         SPACE 1                                                                
ANATAB   DS    0X                                                               
         DC    AL1(SCITFEEA,0),C'  '                                            
         DC    AL1(L'LC$FEEAJ),AL2(LC$FEEAJ-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITBENE,CTRYUSA),C'  '         BENEFIT                      
         DC    AL1(L'LC$BEN),AL2(LC$BEN-TWAD)                                   
         DC    AL1(SCITKSVB,CTRYGER),C'  '         BILLABLE (KSV)               
         DC    AL1(L'LC@BLB),AL2(LC@BLB-TWAD)                                   
         SPACE 1                                                                
         DC    AL1(SCITCOMM,0),C'  '                                            
         DC    AL1(L'LC$CMN),AL2(LC$CMN-TWAD)                                   
         SPACE 1                                                                
         DC    AL1(SCITDEPR,0),C'41'                                            
         DC    AL1(L'LC$DPR),AL2(LC$DPR-TWAD)                                   
         DC    AL1(SCITCDSC,0),C'  '                                            
         DC    AL1(L'LC$CSHDS),AL2(LC$CSHDS-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITXAMT,CTRYGER),C'  '         TRANSFER AMOUNT              
         DC    AL1(L'LC$XFRAM),AL2(LC$XFRAM-TWAD)                               
         DC    AL1(SCITMEXP,CTRYGBR),C'  '         MEMO EXPENSES                
         DC    AL1(L'LC$MEMEX),AL2(LC$MEMEX-TWAD)                               
         DC    AL1(SCITGRNT,0),C'  '               GROSS/NET FOR COKE           
         DC    AL1(L'LC$GRSCK),AL2(LC$GRSCK-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITFEES,0),C'  '                                            
         DC    AL1(L'LC$FEE),AL2(LC$FEE-TWAD)                                   
         SPACE 1                                                                
         DC    AL1(SCITGRSS,0),C'  '                                            
         DC    AL1(L'LC$GRSBD),AL2(LC$GRSBD-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITHOUR,0),C'  '                                            
         DC    AL1(L'LC$HOURS),AL2(LC$HOURS-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITINCA,0),C'  '                                            
         DC    AL1(L'LC$INCM),AL2(LC$INCM-TWAD)                                 
         SPACE 1                                                                
         DC    AL1(SCITJHRS,0),C'  '                                            
         DC    AL1(L'LC$HRSDC),AL2(LC$HRSDC-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITFTAX,CTRYGER),C'  '                                      
         DC    AL1(L'LC$FLTX),AL2(LC$FLTX-TWAD)                                 
         SPACE 1                                                                
         DC    AL1(SCITCLIM,0),C'  '                                            
         DC    AL1(L'LC$RCVCL),AL2(LC$RCVCL-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITGINV,0),C'SI'                                            
         DC    AL1(L'LC$GRSFI),AL2(LC$GRSFI-TWAD)                               
         DC    AL1(SCITMILE,0),C'  '                                            
         DC    AL1(L'LC$MILGE),AL2(LC$MILGE-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITGLEV,CTRYGBR),C'  '                                      
         DC    AL1(L'LC$GRSMV),AL2(LC$GRSMV-TWAD)                               
         DC    AL1(SCITGLEV,CTRYGER),C'  '                                      
         DC    AL1(L'LC$GRSMV),AL2(LC$GRSMV-TWAD)                               
         DC    AL1(SCITGLEV,CTRYCAN),C'  '                                      
         DC    AL1(L'LC$GRSMV),AL2(LC$GRSMV-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITFORD,0),C'  '                                            
         DC    AL1(L'LC$FLYMO),AL2(LC$FLYMO-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITPORD,0),C'  '                                            
         DC    AL1(L'LC$PTYMO),AL2(LC$PTYMO-TWAD)                               
         DC    AL1(SCITIAMT,0),C'ST'                                            
         DC    AL1(L'LC$INSAM),AL2(LC$INSAM-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITFAMT,CTRYHOL),C'  '                                      
         DC    AL1(L'LC$FEEAM),AL2(LC$FEEAM-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITPRAT,0),C'SJ'                                            
         DC    AL1(L'LC$CSTRT),AL2(LC$CSTRT-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITSJXP,0),C'SJ'                                            
         DC    AL1(L'LC$AMPEX),AL2(LC$AMPEX-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITSJHR,CTRYGBR),C'SJ'                                      
         DC    AL1(L'LC$HOURS),AL2(LC$HOURS-TWAD)                               
         DC    AL1(SCITSJHR,CTRYGER),C'SJ'                                      
         DC    AL1(L'LC$HOURS),AL2(LC$HOURS-TWAD)                               
         DC    AL1(SCITSJHR,CTRYGBR),C'1C'                                      
         DC    AL1(L'LC$HOURS),AL2(LC$HOURS-TWAD)                               
         DC    AL1(SCITSJHR,CTRYGER),C'1C'                                      
         DC    AL1(L'LC$HOURS),AL2(LC$HOURS-TWAD)                               
         DC    AL1(SCITTAXP,CTRYGBR),C'SR'                                      
         DC    AL1(L'LC$VATAM),AL2(LC$VATAM-TWAD)                               
         DC    AL1(SCITCHQT,CTRYGBR),C'  '                                      
         DC    AL1(L'LC$TCHKS),AL2(LC$TCHKS-TWAD)                               
         DC    AL1(SCITTAXP,0),C'  '                                            
         DC    AL1(L'LC$TAXPD),AL2(LC$TAXPD-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITKSVU,CTRYGER),C'  '         NON-BILLABLE (KSV)           
         DC    AL1(L'LC@NBLB),AL2(LC@NBLB-TWAD)                                 
         SPACE 1                                                                
         DC    AL1(SCITVEHI,0),C'  '                                            
         DC    AL1(L'LC$VCLS),AL2(LC$VCLS-TWAD)                                 
         SPACE 1                                                                
         DC    AL1(SCITFVAT,CTRYGBR),C'SF'                                      
         DC    AL1(L'LC$VAT),AL2(LC$VAT-TWAD)                                   
         DC    AL1(SCITFVAT,CTRYGER),C'SX'                                      
         DC    AL1(L'LC$VAT),AL2(LC$VAT-TWAD)                                   
         SPACE 1                                                                
         DC    AL1(SCITIVAT,CTRYNOT+CTRYUSA),C'SI'                              
         DC    AL1(L'LC$VAT),AL2(LC$VAT-TWAD)                                   
         DC    AL1(SCITIVAT,CTRYNOT+CTRYUSA),C'2P'                              
         DC    AL1(L'LC$VAT),AL2(LC$VAT-TWAD)                                   
         DC    AL1(SCITNOCD,0),C'  '                                            
         DC    AL1(L'LC$NOCHD),AL2(LC$NOCHD-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITASBF,CTRYGBR),C'SI'                                      
         DC    AL1(L'LC$ASBOF),AL2(LC$ASBOF-TWAD)                               
         DC    AL1(SCITCTOT,0),C'  '                                            
         DC    AL1(L'LC$CHKOC),AL2(LC$CHKOC-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITRECN,0),C'  '                                            
         DC    AL1(L'LC$RCNAM),AL2(LC$RCNAM-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITINVT,0),C'  '                                            
         DC    AL1(L'LC$INVTO),AL2(LC$INVTO-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITMTAX,0),C'  '                                            
         DC    AL1(L'LC$MEMOT),AL2(LC$MEMOT-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITVERN,CTRYHOL),C'  '                                      
         DC    AL1(L'LC$VSCHN),AL2(LC$VSCHN-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITCOKE,0),C'  '                                            
         DC    AL1(L'LC$CODEX),AL2(LC$CODEX-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITUBPC,0),C'  '                                            
         DC    AL1(L'LC$TOLPC),AL2(LC$TOLPC-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITUBAM,0),C'  '                                            
         DC    AL1(L'LC$TOLAM),AL2(LC$TOLAM-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITBLPC,0),C'  '                                            
         DC    AL1(L'LC$TOLPC),AL2(LC$TOLPC-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITBLAM,0),C'  '                                            
         DC    AL1(L'LC$TOLAM),AL2(LC$TOLAM-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITDMAX,0),C'  '                                            
         DC    AL1(L'LC$MAXDB),AL2(LC$MAXDB-TWAD)                               
         SPACE 1                                                                
         DC    AL1(SCITCMAX,0),C'  '                                            
         DC    AL1(L'LC$MAXCB),AL2(LC$MAXCB-TWAD)                               
         SPACE 1                                                                
ANATABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* ACBATWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBATWORK                                                      
         PRINT ON                                                               
TWAD     DSECT                                                                  
         ORG   BASOLY1H                                                         
       ++INCLUDE ACBATA9D                                                       
         ORG   BASOLY1H                                                         
       ++INCLUDE ACBATACD                                                       
         ORG   OSVALS              ** POSTING/LIST STORAGE AREA **              
PLSEQ    DS    XL2                 ITEM SEQUENCE NUMBER                         
PLREF    DS    CL(L'LSTIREF)       ITEM REFERENCE                               
PLDATE   DS    PL(L'LSTIDATE)      ITEM DATE                                    
*                                                                               
PLNARRL  DS    XL1                 LENGTH OF ITEM NARRATIVE                     
PLNARR   DS    CL(L'TRNNARR)       ITEM NARRATIVE                               
PLNARFLG DS    XL1                 NARRATIVE FLAG                               
PLNARFST EQU   X'80'               1ST TIME FOR NARRATIVE CHANGE CHECK          
PLNARCHG EQU   X'08'               NARRATIVE CAN BE CHANGED                     
PLNARINP EQU   X'04'               INPUT NARRATIVE SUPERCEEDS OLD               
PLNARNEW EQU   X'02'               NEW NARRATIVE HAS BEEN VALIDATED             
PLNARSHT EQU   X'01'               SHORT (15 CHR) NARRATIVE FOR W/C 99          
PLNLINM  EQU   4                   MAXIMUM NUMBER OF NARRATIVE LINES            
PLNLINL  EQU   50                  MAXIMUM LENGTH OF NARRATIVE LINE             
*                                                                               
PLVALS   DS    0X                                                               
PLODIS   DS    XL(DISTABL)         OPTION DISPLAY INFORMATION                   
PLVALSL  EQU   *-PLVALS                                                         
*                                                                               
PLVACTWD EQU   3                   CHRS IN VALID ACTION DISPLAY WORD            
         SPACE 1                                                                
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* ACPRORATAD                                                                    
         PRINT OFF                                                              
PRORATD  DSECT                                                                  
       ++INCLUDE ACPRORATAD                                                     
PRORATX  EQU   *                                                                
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031ACBAT62   12/28/17'                                      
         END                                                                    
