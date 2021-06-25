*          DATA SET NEPAY04    AT LEVEL 122 AS OF 11/20/17                      
*PHASE T31304A,+0                                                               
         TITLE 'NETPAK PAY PROGRAM - PRINT OVERLAY - T31304'                    
T31304   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CLPR**,RA,RR=RE                                              
         L     R9,0(R1)                                                         
         USING PAYWRKD,R9                                                       
         USING TWAD,R8                                                          
         L     R7,AOVWORK                                                       
         USING TEMPD,R7                                                         
         ST    RE,MYRELO                                                        
         ST    R1,MYPARM                                                        
*  CHECK FOR SPECIAL ACTION                                                     
         CLC   PAYACT(5),=CL5'P=###'                                            
         BNE   REP100                                                           
         MVC   PAYMSG(L'INVREQ),INVREQ                                          
         B     PRINXIT                                                          
*                                                                               
REP100   BAS   RE,INITSPUL         INITIALIZE SPOOL AND PRINT QUEUE             
*                                                                               
         BAS   RE,INITREP          INIT SPOOL FOR THIS REPORT                   
*                                                                               
         MVC   DATADISP,=H'27'                                                  
*                                                                               
         CLI   PAYDLRS,0           IS COMMERCIAL DISPLAYED                      
         BE    *+8                                                              
         MVI   NBSPLOPT,X'C0'      - SET SPLIT PRODUCT OPTION                   
*                                                                               
         XC    TEMPWORK,TEMPWORK                                                
         LA    RF,TEMPWORK                                                      
         STCM  RF,15,NBADPLST                                                   
*                                                                               
         LA    RE,PRODTBL                                                       
         ST    RE,NBSPLBPN         PROD TABLE FOR NETVALUE                      
*                                                                               
* READ UNIT RECORDS                                                             
*                                                                               
GETUN    GOTO1 VNETIO,DMCB,NETBLOCK                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBPROCUN                                                  
         BNE   GU2                                                              
         MVI   PAYTYPE,0                                                        
         GOTO1 VFILREC             XTRA FILTERING                               
         BNE   GETUN                                                            
         BAS   RE,GOTONE                                                        
GU2      CLI   NBMODE,NBREQLST     TEST FOR END-OF-FILE                         
         BNE   GETUN               NO-GET NEXT RECORD                           
         CLI   RECSW,YES           TEST FOR QUALIFIED RECORDS                   
         BNE   GU4                                                              
         BAS   RE,TOTALS                                                        
         BRAS  RE,ENDSPOOL         CLOSE PRINT QUEUE, PRINT ID                  
         B     PRINXIT                                                          
GU4      MVC   PAYMSG(L'NODATA),NODATA   NO REPORT GENERATED                    
         SPACE 1                                                                
PRINXIT  LA    R2,PAYACTH                                                       
         ST    R2,FADDR            SET CURSOR AT ACTION                         
         XMOD1 1                                                                
*                                                                               
*                                                                               
*                                                                               
******************************************************                          
** PROCESS A UNIT RECORD                                                        
* LOCALS: R5 - ADDRESSES PAYBLOCK                                               
*         R6 - SPOOLD                                                           
*         R3 - COLUMNS IN PRINTLINES                                            
*                                                                               
******************************************************                          
         PRINT GEN                                                              
GOTONE   NTR1                                                                   
         MVI   RECSW,YES           WE HAVE >1 RECORD                            
         L     R6,ASPOOLA                                                       
         USING SPOOLD,R6                                                        
         GOTO1 VSPOOL,DMCB,(R6)    PRINT BLANK LINE                             
         LA    R3,P1                                                            
         USING COLINFO,R3                                                       
         LA    R5,BLOCK            SET UP PAYBLOCK                              
         USING PAYBLKD,R5                                                       
         MVI   PAYFUNCT,PAYTOTS                                                 
*                                                                               
         MVI   SVPAYST,0                                                        
         XC    SRATCOST(24),SRATCOST                                            
         MVI   PAYTYPE,C'T'        GET TIME PAYABLE                             
         GOTO1 VGETPAY,DMCB,PAYWRKD,PAYBLKD                                     
         MVC   TIMEPAYG,PAYGROSS                                                
         MVC   TIMEPAYN,PAYNT                                                   
         OC    SVPAYST,PAYSTAT                                                  
*                                                                               
         MVI   PAYTYPE,C'I'        GET INTEGRATION PAYABLE                      
         GOTO1 VGETPAY,DMCB,PAYWRKD,PAYBLKD                                     
         MVC   INTGPAYG,PAYGROSS                                                
         MVC   INTGPAYN,PAYNT                                                   
         OC    SVPAYST,PAYSTAT                                                  
*                                                                               
         MVI   PAYTYPE,C'R'        GET SPECIAL PAYABLE                          
         GOTO1 VGETPAY,DMCB,PAYWRKD,PAYBLKD                                     
         OC    SVPAYST,PAYSTAT                                                  
*---TEST FOR PRE-EMPT OR MISSING SPOT                                           
         TM    NBUNITST,X'42'                                                   
         BZ    GETO100                                                          
         TM    SVPAYST,PAYABLE                                                  
         BZ    XITMNL                                                           
         XC    NBACTUAL,NBACTUAL                                                
         XC    NBINTEG,NBINTEG                                                  
         OC    SVPAYST,PAYSTAT                                                  
*                                                                               
GETO100  BAS   RE,ACUMRATE                                                      
*                                                                               
         BAS   RE,TESTREP          TEST IF PAYEE EDIT                           
         CLI   DMCB,X'FF'                                                       
         BE    XITMNL              BYPASS RECORD                                
*                                                                               
         L     R0,TIMEPAYG         GET TOTAL PAYABLES                           
         A     R0,INTGPAYG                                                      
         A     R0,SRATPAYG                                                      
         ST    R0,PAYGROSS                                                      
*                                                                               
         L     R0,TIMEPAYN         GET TOTAL PAYABLES                           
         A     R0,INTGPAYN                                                      
         A     R0,SRATPAYN                                                      
         ST    R0,PAYNT                                                         
*                                                                               
         L     R0,PAYGROSS         UPDATE REPORT TOTALS                         
         A     R0,TOTG                                                          
         ST    R0,TOTG                                                          
         L     R0,PAYNT                                                         
         A     R0,TOTN                                                          
         ST    R0,TOTN                                                          
         L     R0,NBACTUAL                                                      
         A     R0,TOTACT                                                        
         ST    R0,TOTACT                                                        
         L     R0,NBINTEG                                                       
         A     R0,TOTINT                                                        
         ST    R0,TOTINT                                                        
         L     R0,SRATCOST                                                      
         A     R0,TOTSRAT                                                       
         ST    R0,TOTSRAT                                                       
*                                                                               
*                                                                               
DODATE   BAS   RE,PRINDATE         PRINT DATE GROUP                             
         BAS   RE,PRINPROG         PRINT PROG GROUP                             
*                                                                               
         EDIT  NBLEN,(3,CLLEN)                                                  
         MVC   CLDAYP,NBACTNDP                                                  
*                                                                               
         CLI   PAYDLRS,0           IS COMMERCIAL DISPLAYED                      
         BE    DONSPRD                                                          
         LA    R4,CLPRD1                                                        
         MVC   0(3,R4),NBSPLPR3                                                 
**       MVC   DISPRD,NBSPLPRN                                                  
**       BAS   RE,PRNTPRD                                                       
         B     DOCOST                                                           
*                                                                               
DONSPRD  MVC   PRDLINE,SPACES                                                   
         OC    NBPRDLST,NBPRDLST   ARE THERE MORE THEN 2 PRODS                  
         BNZ   DOPRD100            YES SPECIAL OUTPUT                           
*                                                                               
         LA    R4,CLPRD1                                                        
**       MVC   DISPRD,NBPRD                                                     
**       BAS   RE,PRNTPRD                                                       
         MVC   0(3,R4),NBPR1CL3                                                 
*                                                                               
***      OC    NBPRD2,NBPRD2                                                    
***      BZ    DOCOST                                                           
         OC    NBPR2CL3,NBPR2CL3                                                
         BZ    DOCOST                                                           
         CLI   2(R4),C' '                                                       
         BE    *+8                                                              
         LA    R4,1(R4)                                                         
         LA    R4,2(R4)                                                         
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
***      MVC   DISPRD,NBPRD2                                                    
***      BAS   RE,PRNTPRD                                                       
         MVC   0(3,R4),NBPR2CL3                                                 
         B     DOCOST                                                           
*                                                                               
*--SPECIAL PRODUCT OUTPUT ROUTINE WHEN MORE THE 2 PRODUCTS EXIST                
DOPRD100 DS    0H                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'19',NBAIO),0                       
         CLI   12(R1),0                                                         
         BNE   DOPRD150                                                         
         L     R1,12(R1)                                                        
         USING NUPDED,R1                                                        
         ZIC   RE,NUPDELEN                                                      
         S     RE,=F'3'                                                         
         USING NUPDED,R1                                                        
         LA    R1,3(R1)            R1->3 CHAR PROD                              
         LA    R4,CLPRD1                                                        
         B     *+12                                                             
DOPRD140 MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
         MVC   0(3,R4),0(R1)                                                    
         LA    R1,7(R1)            BUMP TO NEXT PROD                            
         LA    R4,3(R4)                                                         
         BCT   RE,DOPRD140                                                      
         B     DOCOST                                                           
**                                                                              
DOPRD150 LA    R4,CLPRD1                                                        
         MVC   DISPRD,NBPRDLST                                                  
         BAS   RE,PRNTPRD                                                       
*                                                                               
         OC    NBPRDLST+1(1),NBPRDLST+1                                         
         BZ    DOCOST                                                           
         CLI   2(R4),C' '                                                       
         BE    *+8                                                              
         LA    R4,1(R4)                                                         
         LA    R4,2(R4)                                                         
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
         MVC   DISPRD,NBPRDLST+1                                                
         BAS   RE,PRNTPRD                                                       
*                                                                               
DOPRD200 ZIC   RF,NBPRDNO                                                       
         SH    RF,=H'2'                                                         
         LTR   RF,RF                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    R2,NBPRDLST+2                                                    
         LA    R4,PRDLINE                                                       
*                                                                               
DOPRD220 MVC   DISPRD,0(R2)                                                     
         BAS   RE,PRNTPRD                                                       
         CLI   2(R4),C' '                                                       
         BE    *+8                                                              
         LA    R4,1(R4)                                                         
         LA    R4,2(R4)                                                         
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
         LA    R2,1(R2)                                                         
         BCT   RF,DOPRD220                                                      
         BCTR  R4,0                                                             
         MVI   0(R4),C' '          REMOVE LAST SLASH                            
*                                                                               
DOCOST   XC    UNITCOST(12),UNITCOST                                            
         OC    NBACTUAL,NBACTUAL   CK FOR ZERO COST                             
         BNZ   DOC2                                                             
         TM    NBUNITST,X'20'      TEST IF COST ALLOCATED                       
         BO    DOC1                YES-ZERO COST                                
         MVC   CLACT+4(6),=C'*NONE*' FLAG MISSING ACTUAL COST                   
         B     DOC4                                                             
*                                                                               
DOC1     MVC   CLACT+5(2),=C'$0'      IF SO, PRINT $0                           
         B     DOC4                                                             
*                                                                               
DOC2     CLC   NBACTUAL,=XL4'3B9AC9FF'    > $10M                                
         BH    DOC2A                                                            
         EDIT  NBACTUAL,(10,CLACT),2,FLOAT=-                                    
         B     DOC4                                                             
DOC2A    EDIT  NBACTUAL,(10,TEMPCOST),FLOAT=-                                   
         MVC   CLACT+2(8),TEMPCOST                                              
*                                                                               
DOC4     CLC   NBINTEG,=XL4'3B9AC9FF'    > $10M                                 
         BH    DOC4A                                                            
         EDIT  NBINTEG,(10,CLICOS),2,FLOAT=-                                    
         B     DOC5                                                             
DOC4A    EDIT  NBINTEG,(10,TEMPCOST),FLOAT=-                                    
         MVC   CLICOS+2(8),TEMPCOST                                             
*                                                                               
DOC5     L     R2,NBACTUAL                                                      
         A     R2,NBINTEG                                                       
         ST    R2,UNITCOST                                                      
         MVC   CLCTIME(4),=C'TIME'                                              
         MVC   CLINT(3),=C'INT'                                                 
*                                                                               
DOBILL   GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'10',NBAIO),0                       
         CLI   12(R1),0            IF NOT BILLED SKIP PRINTING                  
         BE    DOBILL10                                                         
*                                                                               
* CHECK NEW BILL RECORDS                                                        
*                                                                               
         XC    DMWORK(96),DMWORK                                                
         LA    RE,DMWORK                                                        
         STCM  RE,15,NBABILRD       ADDRESS OF BILL DSECT IN NETBLOCK           
         USING NBLBILLD,RE                                                      
         MVC   NBLUNAIO,NBAIO                                                   
         OI    NBLFUNC,NBLBLD                                                   
         GOTO1 VBILRDR,DMCB,NETBLOCK                                            
         LA    RE,DMWORK                                                        
         TM    NBLFUNC,NBLBILD                                                  
         BZ    DOCLEAR                                                          
DOBILL10 MVC   CLB(1),=C'*'        BILLED                                       
*                                                                               
DOCLEAR  EQU   *                                                                
         CLC   NBPAYTGR,=XL4'3B9AC9FF'    > $10M                                
         BH    DOCL15                                                           
         EDIT  NBPAYTGR,(10,CLCLR),2,FLOAT=-                                    
         B     DOCL20                                                           
DOCL15   EDIT  NBPAYTGR,(10,TEMPCOST),FLOAT=-                                   
         MVC   CLCLR+2(8),TEMPCOST                                              
*                                                                               
DOCL20   CLC   NBPAYIGR,=XL4'3B9AC9FF'    > $10M                                
         BH    DOCL25                                                           
         EDIT  NBPAYIGR,(10,CLICLR),2,FLOAT=-                                   
         B     DOCL30                                                           
DOCL25   EDIT  NBPAYIGR,(10,TEMPCOST),FLOAT=-                                   
         MVC   CLICLR+2(8),TEMPCOST                                             
*                                                                               
DOCL30   L     R2,NBPAYTGR         CLEARED TIME GROSS                           
         A     R2,NBPAYIGR         PLUS CLEARED INTEGRATION                     
         ST    R2,UNITCLRG                                                      
*                                                                               
         CLC   NBPAYTNT,=XL4'3B9AC9FF'    > $10M                                
         BH    DOCL45                                                           
         EDIT  NBPAYTNT,(10,CLCLRN),2,FLOAT=-                                   
         B     DOCL50                                                           
DOCL45   EDIT  NBPAYTNT,(10,TEMPCOST),FLOAT=-                                   
         MVC   CLCLRN+2(8),TEMPCOST                                             
*                                                                               
DOCL50   CLC   NBPAYINT,=XL4'3B9AC9FF'    > $10M                                
         BH    DOCL55                                                           
         EDIT  NBPAYINT,(10,CLICLRN),2,FLOAT=-                                  
         B     DOCL60                                                           
DOCL55   EDIT  NBPAYINT,(10,TEMPCOST),FLOAT=-                                   
         MVC   CLICLRN+2(8),TEMPCOST                                            
*                                                                               
DOCL60   L     R2,NBPAYTNT         CLEARED TIME NET                             
         A     R2,NBPAYINT         PLUS CLEARED INTEGRATION                     
         ST    R2,UNITCLRN                                                      
*                                                                               
DOPAY    CLI   PAYRSN,0            TEST FOR PAYABLE ERROR                       
         BE    DOP10               NO                                           
         GOTO1 VGETRSN,DMCB,PAYBLKD,CLPAYGR                                     
         B     DOFILT                                                           
*                                                                               
DOP10    CLC   TIMEPAYG,=XL4'3B9AC9FF'    > $10M                                
         BH    DOP15                                                            
         EDIT  TIMEPAYG,(10,CLPAYGR),2,FLOAT=-                                  
         B     DOP20                                                            
DOP15    EDIT  TIMEPAYG,(10,TEMPCOST),FLOAT=-                                   
         MVC   CLPAYGR+2(8),TEMPCOST                                            
*                                                                               
DOP20    CLC   INTGPAYG,=XL4'3B9AC9FF'    > $10M                                
         BH    DOP25                                                            
         EDIT  INTGPAYG,(10,CLIPAYGR),2,FLOAT=-                                 
         B     DOP30                                                            
DOP25    EDIT  INTGPAYG,(10,TEMPCOST),FLOAT=-                                   
         MVC   CLIPAYGR+2(8),TEMPCOST                                           
*                                                                               
DOP30    CLC   TIMEPAYN,=XL4'3B9AC9FF'    > $10M                                
         BH    DOP35                                                            
         EDIT  TIMEPAYN,(10,CLPAYNT),2,FLOAT=-                                  
         B     DOP40                                                            
DOP35    EDIT  TIMEPAYN,(10,TEMPCOST),FLOAT=-                                   
         MVC   CLPAYNT+2(8),TEMPCOST                                            
*                                                                               
DOP40    CLC   INTGPAYN,=XL4'3B9AC9FF'    > $10M                                
         BH    DOP45                                                            
         EDIT  INTGPAYN,(10,CLIPAYNT),2,FLOAT=-                                 
         B     DOFILT                                                           
DOP45    EDIT  INTGPAYN,(10,TEMPCOST),FLOAT=-                                   
         MVC   CLIPAYNT+2(8),TEMPCOST                                           
*                                                                               
DOFILT   MVC   CLFILT,UFILTER      DISPLAY UNIT FILTER                          
*                                                                               
         OC    NBSREP,NBSREP       TEST FOR SPECIAL REP                         
         BZ    DOCOMML             NO                                           
         SR    R0,R0                                                            
         ICM   R0,3,NBSREP                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CLREP,DUB           REP CODE                                     
         MVC   CLSREP,=C'SPEC REP='                                             
*                                                                               
DOCOMML  CLI   PAYDLRS,0           IS COMMERCIAL DISPLAYED                      
         BE    *+8                                                              
         BAS   RE,PRNTCML                                                       
*                                                                               
DB4      GOTO1 VSPOOL,DMCB,(R6)                                                 
         BAS   RE,SRTPRNT                                                       
*                                                                               
XITMNL   XIT1                                                                   
         EJECT                                                                  
**********************************************************                      
* SRTPRNT - PRINTS THE MULTIPLE SPECIAL RATE LINES AND THE                      
* UNIT TOTAL LINE FOR EACH UNIT. INPUT = SRATTAB                                
**********************************************************                      
SRTPRNT  NTR1                                                                   
         LA    R2,SRATTAB                                                       
         LA    R4,16                                                            
*                                                                               
SRTP100  CLI   0(R2),0                                                          
         BE    SRTP400                                                          
         CLI   0(R2),X'FF'                                                      
         BE    SRTP300                                                          
*--MOVE DESCRIPTION OUT TO PRINT LINE                                           
         LA    RE,DESCTAB                                                       
         LA    RF,12                                                            
SRTP120  CLC   0(1,R2),0(RE)                                                    
         BE    SRTP125                                                          
         LA    RE,6(RE)                                                         
         BCT   RF,SRTP120                                                       
         DC    H'0'                                                             
SRTP125  MVC   CLSRA,1(RE)                                                      
         MVC   CLSPRDS(15),PRDLINE                                              
         MVC   PRDLINE,SPACES                                                   
*                                                                               
         CLC   28(4,R2),=XL4'3B9AC9FF'    > $10M                                
         BH    STRP145                                                          
         EDIT  (4,28(R2)),(10,CLSACT),2,FLOAT=-                                 
         B     STRP150                                                          
STRP145  EDIT  (4,28(R2)),(10,TEMPCOST),FLOAT=-                                 
         MVC   CLSACT+2(8),TEMPCOST                                             
*                                                                               
STRP150  CLC   12(4,R2),=XL4'3B9AC9FF'    > $10M                                
         BH    STRP155                                                          
         EDIT  (4,12(R2)),(10,CLSCLR),2,FLOAT=-                                 
         B     STRP160                                                          
STRP155  EDIT  (4,12(R2)),(10,TEMPCOST),FLOAT=-                                 
         MVC   CLSCLR+2(8),TEMPCOST                                             
*                                                                               
STRP160  CLC   16(4,R2),=XL4'3B9AC9FF'    > $10M                                
         BH    STRP165                                                          
         EDIT  (4,16(R2)),(10,CLSCLRN),2,FLOAT=-                                
         B     STRP170                                                          
STRP165  EDIT  (4,16(R2)),(10,TEMPCOST),FLOAT=-                                 
         MVC   CLSCLRN+2(8),TEMPCOST                                            
*                                                                               
STRP170  CLC   20(4,R2),=XL4'3B9AC9FF'    > $10M                                
         BH    STRP175                                                          
         EDIT  (4,20(R2)),(10,CLSPAYGR),2,FLOAT=-                               
         B     STRP180                                                          
STRP175  EDIT  (4,20(R2)),(10,TEMPCOST),FLOAT=-                                 
         MVC   CLSPAYGR+2(8),TEMPCOST                                           
*                                                                               
STRP180  CLC   24(4,R2),=XL4'3B9AC9FF'    > $10M                                
         BH    STRP185                                                          
         EDIT  (4,24(R2)),(10,CLSPAYNT),2,FLOAT=-                               
         B     STRP190                                                          
STRP185  EDIT  (4,24(R2)),(10,TEMPCOST),FLOAT=-                                 
         MVC   CLSPAYNT+2(8),TEMPCOST                                           
*                                                                               
STRP190  CLC   1(3,R2),=XL3'404040'                                             
         BE    SRTP200                                                          
         OC    1(3,R2),1(R2)                                                    
         BZ    SRTP200                                                          
         MVC   CLSRSREP,=C'SPEC REP='                                           
         MVC   CLSRREP,1(R2)                                                    
SRTP200  GOTO1 VSPOOL,DMCB,(R6)                                                 
*--ADD TO UNIT TOTALS                                                           
         L     RE,4(R2)                                                         
         A     RE,UNITCOST                                                      
         ST    RE,UNITCOST                                                      
*                                                                               
         L     RE,12(R2)                                                        
         A     RE,UNITCLRG                                                      
         ST    RE,UNITCLRG                                                      
*                                                                               
         L     RE,16(R2)                                                        
         A     RE,UNITCLRN                                                      
         ST    RE,UNITCLRN                                                      
*                                                                               
SRTP300  LA    R2,32(R2)                                                        
         BCT   RE,SRTP100                                                       
*                                                                               
SRTP400  MVC   CLTPRDS(15),PRDLINE                                              
         MVC   CLTOT(5),=C'TOTAL'                                               
         L     R2,UNITCOST                                                      
         CLC   UNITCOST,=XL4'3B9AC9FF'    > $10M                                
         BH    STRP415                                                          
         EDIT  (R2),(10,CLTACT),2,FLOAT=-                                       
         B     STRP420                                                          
STRP415  EDIT  (R2),(10,TEMPCOST),FLOAT=-                                       
         MVC   CLTACT+2(8),TEMPCOST                                             
*                                                                               
STRP420  ST    R2,SAVUTOT          PASS TOTAL TO CALCULATE CLR                  
         A     R2,TOTCOST          UPDATE TOTAL COST BUCKET                     
         ST    R2,TOTCOST                                                       
*                                                                               
         L     R2,UNITCLRG         PLUS CLEARED INTEGRATION                     
         CLC   UNITCLRG,=XL4'3B9AC9FF'    > $10M                                
         BH    STRP425                                                          
         EDIT  (R2),(10,CLTCLR),2,FLOAT=-                                       
         B     STRP430                                                          
STRP425  EDIT  (R2),(10,TEMPCOST),FLOAT=-                                       
         MVC   CLTCLR+2(8),TEMPCOST                                             
*                                                                               
STRP430  A     R2,TOTCLEAR         UPDATE TOTAL CLEARED BUCKET                  
         ST    R2,TOTCLEAR                                                      
*                                                                               
         L     R2,UNITCLRN         PLUS CLEARED INTEGRATION                     
         CLC   UNITCLRN,=XL4'3B9AC9FF'    > $10M                                
         BH    STRP435                                                          
         EDIT  (R2),(10,CLTCLRN),2,FLOAT=-                                      
         B     STRP440                                                          
STRP435  EDIT  (R2),(10,TEMPCOST),FLOAT=-                                       
         MVC   CLTCLRN+2(8),TEMPCOST                                            
*                                                                               
STRP440  A     R2,TOTCLRN          UPDATE CLEARED NET BUCKET                    
         ST    R2,TOTCLRN                                                       
*                                                                               
         CLC   PAYGROSS,=XL4'3B9AC9FF'    > $10M                                
         BH    STRP445                                                          
         EDIT  PAYGROSS,(10,CLTPAYGR),2,FLOAT=-                                 
         B     STRP450                                                          
STRP445  EDIT  PAYGROSS,(10,TEMPCOST),FLOAT=-                                   
         MVC   CLTPAYGR+2(8),TEMPCOST                                           
*                                                                               
STRP450  CLC   PAYNT,=XL4'3B9AC9FF'    > $10M                                   
         BH    STRP455                                                          
         EDIT  PAYNT,(10,CLTPAYNT),2,FLOAT=-                                    
         B     STRPX                                                            
STRP455  EDIT  PAYNT,(10,TEMPCOST),FLOAT=-                                      
         MVC   CLTPAYNT+2(8),TEMPCOST                                           
*                                                                               
STRPX    DS    0H                                                               
         GOTO1 VSPOOL,DMCB,(R6)                                                 
         XIT1                                                                   
*                                                                               
DESCTAB  DC    CL6'UCUTIN'                                                      
         DC    CL6'BBLKOT'                                                      
         DC    CL6'SCOPSP'                                                      
         DC    CL6'XTAX  '                                                      
         DC    CL6'ESECT '                                                      
         DC    CL6'AADMIN'                                                      
         DC    CL6'LLTECH'                                                      
         DC    CL6'OOTHER'                                                      
         DC    CL6'DDLADJ'                                                      
         DC    CL6'QEC   '                                                      
         DC    CL6'CBRTAC'                                                      
         DC    CL6'MTRACR'                                                      
         PRINT NOGEN                                                            
*******************************************************                         
         EJECT                                                                  
*******************************************************                         
* PRINDATE - PRINT DATE GROUP IN PROPER PRINT POSITION                          
* INPUT - NBSEQ - 'D' OR BLANK FOR DATES FIRST                                  
*                 'P' OR 'Q' FOR PROGS FIRST                                    
*         R3 - CURRENT PRINT LINE                                               
*         DATOFF - OFFSET OF DATE GROUP (SET IN INIT)                           
* LOCAL - R4 - START LOC OF DATE GROUP                                          
*******************************************************                         
PRINDATE NTR1                                                                   
         LR    R4,R3                                                            
         A     R4,DATOFF           IF DATE SEQ THEN PRINT DATES 1ST             
*                                                                               
         USING DATGRP,R4                                                        
         GOTO1 VDATCON,DMCB,(2,NBACTDAT),(7,CLDATE)                             
         MVC   CLSUB,NBSUBOUT                                                   
         GOTO1 VUNTIME,DMCB,NBTIME,CLPRTM                                       
         MVC   CLDAY(L'NBDAYNAM),NBDAYNAM                                       
         XIT1                                                                   
         DROP  R4                                                               
*******************************************************                         
         EJECT                                                                  
*******************************************************                         
* PRNTPRD - PRINT 3 CHAR PRODUCT CODE                                           
* INPUT - DISPRP - PRODUCT CODE 0/FF = UNALLOCATED                              
*         R4 - CURRENT PRINT LINE                                               
* LOCAL - R1 - CLIENTR LIST OF PRODUCTS                                         
*******************************************************                         
PRNTPRD  NTR1                                                                   
         CLI   DISPRD,X'FF'                                                     
         BE    *+14                                                             
         OC    DISPRD,DISPRD       TEST FOR UNALLOCATED UNIT                    
         BNZ   *+14                NO                                           
         MVC   0(7,R4),=C'*UNALL*'                                              
         B     PPEXT                                                            
*                                                                               
         L     R1,ACLIREC          ADDRESS OF CLIENT RECORD                     
         USING CLTHDR,R1                                                        
         LA    R2,CLIST            LOOK UP PRODUCT CODE                         
         LA    R0,220                                                           
PRD1LOOP OC    0(4,R2),0(R2)       IF END-OF-LIST THEN EXIT                     
         BZ    PRDNFND                                                          
         CLC   3(1,R2),DISPRD                                                   
         BE    PRD1FND                                                          
         LA    R2,4(R2)            NEXT IN LIST                                 
         BCT   R0,PRD1LOOP                                                      
* CHECK SECOND PRODUCT TABLE                                                    
         LA    R2,CLIST2           LOOK UP PRODUCT CODE                         
         LA    R0,35                                                            
PRD2LOOP OC    0(4,R2),0(R2)       IF END-OF-LIST THEN EXIT                     
         BZ    PRDNFND                                                          
         CLC   3(1,R2),DISPRD                                                   
         BE    PRD1FND                                                          
         LA    R2,4(R2)            NEXT IN LIST                                 
         BCT   R0,PRD2LOOP                                                      
*                                                                               
PRDNFND  DC    H'0'                TAKE A HIT IF NOT FOUND                      
*                                                                               
PRD1FND  MVC   0(3,R4),0(R2)                                                    
*                                                                               
PPEXT    XIT1                                                                   
         DROP  R1                                                               
*******************************************************                         
         EJECT                                                                  
*******************************************************                         
* PRNTCML - PRINT 8 CHAR COMMERCIAL                                             
* LOCAL - R4 - A(COMMERCIAL PAY ELEMENT                                         
*******************************************************                         
PRNTCML  NTR1                                                                   
         XC    DMCB(8),DMCB        0 FOR SECOND PARM                            
CML100   GOTO1 MYGETEL,DMCB,(X'24',NBAIO)                                       
         BZ    CMLEXT              NOT FOUND                                    
         L     R4,4(R1)                                                         
         USING NUPCELD,R4                                                       
* IF NBSPLPRN IS A INTERNAL NETVALUE FUDGE - THEN THIS IS AN OVERFLOW           
* PROD AND WE MUST COMPARE AGAINST 3 CHAR ACTER PROD                            
         TM    NBINDS6,NBI6XOVP    IS NBSPLPRN A NETVALUE FUDGE?                
         BNO   CML200                                                           
         CLI   NUPCLEN,15          IS IT ELEM WITH 3 CHAR PROD?                 
         BNE   CML100                                                           
         CLC   NBSPLPR3,NUPCAPRD                                                
         BNE   CML100                                                           
         B     CML500                                                           
*                                                                               
CML200   CLI   NUPCLEN,15           X'24' ELEM HAS 3 CHAR PROD?                 
         BNE   CML300               NO                                          
         CLC   NBSPLPR3,NUPCAPRD    YES                                         
         BNE   CML100                                                           
         B     CML500                                                           
CML300   CLC   NBSPLPRN,NUPCPRD                                                 
         BNE   CML100                                                           
CML500   MVC   CLCOMML,NUPCCML                                                  
CMLEXT   XIT1                                                                   
         DROP  R4                                                               
*******************************************************                         
         EJECT                                                                  
*******************************************************                         
* ACUMRATE - GETS ALL SPECIAL REP COSTS                                         
*******************************************************                         
*                                                                               
ACUMRATE NTR1                                                                   
         XCEF  SRATTAB,512                                                      
ACUM050  BAS   RE,GETSPRAT                                                      
         CLI   DMCB+12,X'FF'                                                    
         BE    ACUM100                                                          
         L     R5,DMCB+12                                                       
         BAS   RE,CHKTAB                                                        
         B     ACUM050                                                          
ACUM100  BAS   RE,GETSPAY                                                       
         BAS   RE,CALCLEAR                                                      
         BAS   RE,CALSTOT                                                       
         XIT1                                                                   
*                                                                               
*******************************************************                         
* GETSPRAT - CHECKS SPECIAL REP CODES ON SPECIAL RATE ELEMENTS                  
*******************************************************                         
*                                                                               
GETSPRAT NTR1                                                                   
         CLI   0(R5),X'03'                                                      
         BE    GTSR050                                                          
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'03',NBAIO),0                       
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BNE   GTSR080             YES                                          
         L     R5,12(R1)                                                        
         B     GTSR100                                                          
         USING NUSPRD,R5                                                        
GTSR050  ZIC   RE,NUSPRLEN                                                      
         AR    R5,RE                                                            
         CLI   NUSPREL,X'03'                                                    
         BE    GTSR100                                                          
*                                                                               
GTSR080  MVI   DMCB+12,X'FF'                                                    
         B     GTSREX                                                           
*                                                                               
GTSR100  ST    R5,DMCB+12                                                       
*                                                                               
GTSREX   XIT1                                                                   
         DROP  R5                                                               
*******************************************************                         
* CHKTAB - CHECKS TO SEE IF ELEMENT IS ALREADY IN TABLE                         
*******************************************************                         
*                                                                               
CHKTAB   NTR1                                                                   
         L     R5,DMCB+12                                                       
         USING NUSPRD,R5                                                        
         LA    R2,SRATTAB                                                       
         LA    R4,16                                                            
CHKTB050 CLI   0(R2),0                                                          
         BE    CHKTB200                                                         
         CLC   0(1,R2),NUSPRTYP    IS THERE A MATCH ON RATE TYPE                
         BNE   CHKTB100                                                         
         CLC   1(3,R2),NUSPRREP    IS THERE A MATCH ON REP                      
         BE    CHKTB300                                                         
CHKTB100 LA    R2,32(R2)                                                        
         BCT   R4,CHKTB050                                                      
         DC    H'0'                                                             
*                                                                               
CHKTB200 MVI   DMCB,X'00'                                                       
         MVC   0(1,R2),NUSPRTYP                                                 
         MVC   1(3,R2),NUSPRREP                                                 
*---TEST FOR PRE-EMPT OR MISSING SPOT                                           
         TM    NBUNITST,X'42'                                                   
         BNZ   CHKTBEX                                                          
*                                                                               
CHKTB300 ICM   RF,15,NUSPRAMT                                                   
         ICM   RE,15,NUSPRAMT                                                   
         A     RE,28(R2)                                                        
         ST    RE,28(R2)           RAW GROSS DOLLARS                            
*                                                                               
*        ICM   R3,15,NUSPRAMT                                                   
*        CLI   NUSPRCOM,C'C'                                                    
*        BE    CHKTB350                                                         
*        OC    NBRTTYPE,NBRTTYPE                                                
*        BZ    CHKTB400                                                         
CHKTB350 BAS   RE,FINDNET                                                       
CHKTB400 ICM   RE,15,DMCB2+12                                                   
         A     RE,4(R2)                                                         
         ST    RE,4(R2)            GROSS DOLLARS                                
         ICM   RE,15,DMCB2+16                                                   
         A     RE,8(R2)                                                         
         ST    RE,8(R2)            NET DOLLARS                                  
*                                                                               
CHKTBEX  XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
**************************************************                              
* GETSPAY -  GETS ALL THE PAYABLE INFORMATION FOR THE SPECIAL                   
* RATE ELEMENTS                                                                 
**************************************************                              
GETSPAY  NTR1                                                                   
         LA    R2,BLOCK            SET UP PAYBLOCK                              
         USING PAYBLKD,R2                                                       
         MVC   SPECBIT,SPECREP                                                  
         MVC   SPECBREP,SRATREP                                                 
         XC    SPECREP,SPECREP                                                  
         LA    R4,SRATTAB                                                       
         LA    R5,16                                                            
*                                                                               
GETSP050 CLI   0(R4),0                                                          
         BE    GETSPEX                                                          
         MVC   SRATREP,1(R4)                                                    
         MVC   PAYTYPE,0(R4)                                                    
         OI    PAYFLAGS,FACINPUT             FORCE ACT COST INPUT               
         GOTO1 VGETPAY,DMCB,PAYWRKD,PAYBLKD                                     
         MVC   20(4,R4),PAYGROSS                                                
         MVC   24(4,R4),PAYNT                                                   
         LA    R4,32(R4)                                                        
         BCT   R5,GETSP050                                                      
*                                                                               
GETSPEX  MVC   SPECREP,SPECBIT                                                  
         MVC   SRATREP,SPECBREP                                                 
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
**************************************************                              
* CALCLEAR -  GETS ALL THE CLEARED INFORMATION FOR THE SPECIAL                  
* RATE ELEMENTS                                                                 
**************************************************                              
CALCLEAR NTR1                                                                   
         LA    R4,SRATTAB                                                       
         LA    R5,16                                                            
*                                                                               
CALCL050 CLI   0(R4),0                                                          
         BE    CALCL100                                                         
         L     RE,4(R4)            CALCULATE GROSS CLEARED                      
         S     RE,20(R4)                                                        
         ST    RE,12(R4)                                                        
*                                                                               
         L     RE,8(R4)            CALCULATE NET CLEARED                        
         S     RE,24(R4)                                                        
         ST    RE,16(R4)                                                        
         LA    R4,32(R4)                                                        
         BCT   R5,CALCL050                                                      
*---TEST FOR PRE-EMPT OR MISSING SPOT                                           
CALCL100 TM    NBUNITST,X'42'                                                   
         BZ    CALCLEX                                                          
*---IF SPOT IS MISSED CLEARED=PAYABLE*(-1)                                      
         LA    R4,SRATTAB                                                       
         LA    R5,16                                                            
CALCL200 CLI   0(R4),0                                                          
         BE    CALCLEX                                                          
         L     RE,20(R4)           CALCULATE GROSS CLEARED                      
         LCR   RF,RE                                                            
         ST    RF,12(R4)                                                        
*                                                                               
         L     RE,24(R4)           CALCULATE NET CLEARED                        
         LCR   RF,RE                                                            
         ST    RF,16(R4)                                                        
         LA    R4,32(R4)                                                        
         BCT   R5,CALCL200                                                      
*                                                                               
CALCLEX  XIT1                                                                   
         EJECT                                                                  
*******************************************************                         
* CALSTOT - CHECKS SPECIAL REP CODES ON SPECIAL                                 
* RATE ELEMENTS AND ACCUMULATES THE TOTALS                                      
*******************************************************                         
*                                                                               
CALSTOT  NTR1                                                                   
         LA    R2,BLOCK            SET UP PAYBLOCK                              
         USING PAYBLKD,R2                                                       
         LA    RE,SRATTAB                                                       
         LA    RF,16                                                            
*                                                                               
CALS100  CLI   0(RE),0                                                          
         BE    CALSEX                                                           
         TM    SPECREP,X'80'       TEST FOR ANY SPEC REP FILTER                 
         BO    CALS200             YES                                          
         CLC   SRATREP,1(RE)       TEST FOR MATCH ON SPECIAL REP                
         BE    CALS300             YES                                          
CALS150  MVI   0(RE),X'FF'         YES                                          
         LA    RE,32(RE)                                                        
         BCT   RF,CALS100                                                       
*                                                                               
CALS200  CLC   1(3,RE),=XL3'404040'   TEST FOR ANY SPECIAL REP                  
         BE    CALS150                                                          
         OC    1(3,RE),1(RE)                                                    
         BZ    CALS150                                                          
         B     CALS300             NO-SKIP RECORD                               
*-- CALCULATE TOTALS                                                            
CALS300  L     R0,4(RE)                                                         
         A     R0,SRATCOST                                                      
         ST    R0,SRATCOST                                                      
*                                                                               
         L     R0,8(RE)                                                         
         A     R0,SRATNET                                                       
         ST    R0,SRATNET                                                       
*                                                                               
         L     R0,12(RE)                                                        
         A     R0,SRATCLRG                                                      
         ST    R0,SRATCLRG                                                      
*                                                                               
         L     R0,16(RE)                                                        
         A     R0,SRATCLRN                                                      
         ST    R0,SRATCLRN                                                      
*                                                                               
         L     R0,20(RE)                                                        
         A     R0,SRATPAYG                                                      
         ST    R0,SRATPAYG                                                      
*                                                                               
         L     R0,24(RE)                                                        
         A     R0,SRATPAYN                                                      
         ST    R0,SRATPAYN                                                      
*                                                                               
         LA    RE,32(RE)                                                        
         BCT   RF,CALS100                                                       
*                                                                               
CALSEX   XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
**************************************************                              
* SUB-ROUTINE TO CALCULATE NET - AT ENTRY RF CONTAINS GROSS, ON EXIT            
* R3 CONTAINS RESULT                                                            
**************************************************                              
FINDNET  LR    R3,RE                                                            
*---SET UP DMCB FOR NETNET CALL                                                 
         LA    RE,DMCB2+8                                                       
         ST    RE,DMCB2                                                         
         ST    RF,DMCB2+8                                                       
*                                                                               
         MVI   DMCB2,C'F'          NO COMM GROSS=NET                            
         CLI   4(R5),C'C'                                                       
         BE    *+12                                                             
         CLI   4(R5),C'Y'                                                       
         BNE   FINDN050                                                         
*                                                                               
         MVC   DMCB2(1),NBRTTYPE   SET RATE TYPE                                
         CLI   NBSDRTCV,0                                                       
         BE    FINDN050                                                         
         CLI   NBSDRTCV,C'A'                                                    
         BE    FINDN050                                                         
         MVI   DMCB2,0  '          SET TO DEFAULT 85%                           
*                                                                               
FINDN050 LA    RE,DMCB2+12                                                      
         ST    RE,DMCB2+4                                                       
         GOTO1 VNETNET,DMCB2                                                    
         LR    RE,R3                                                            
         ICM   R3,15,DMCB2+16                                                   
         BR    RE                                                               
         EJECT                                                                  
*******************************************************                         
* PRINPROG - PRINT PROG GROUP IN PROPER PRINT POSITION                          
* INPUT - NBSEQ - 'D' OR BLANK FOR DATES FIRST                                  
*                 'P' OR 'Q' FOR PROGS FIRST                                    
*         R3 - CURRENT PRINT LINE                                               
* LOCAL - R4 - START LOC OF PROG GROUP                                          
*******************************************************                         
PRINPROG NTR1                                                                   
         LR    R4,R3                                                            
         A     R4,PRGOFF                                                        
*                                                                               
         USING PRGGRP,R4                                                        
         MVC   CLPRNM,NBPROGNM                                                  
         MVC   CLPRCD,NBACTPRG                                                  
         MVI   CLSLSH1,C'/'                                                     
         EDIT  NBACTEST,(3,CLEST)                                               
         MVI   CLSLSH2,C'/'                                                     
         EDIT  NBPACK,(3,CLPAK)                                                 
         XIT1                                                                   
         DROP  R4                                                               
*******************************************************                         
         EJECT                                                                  
*******************************************************                         
* MY GETEL EXPECTS PARMS SET AS FOLLOWS:                                        
* PARM1 BYTE 0   = ELEMNET CODE                                                 
*       BYTE 1-3 = A(RECORD)                                                    
* PARM2          = LAST ELEMENT FOUND OR 0 TO INDICATE FIRST RECORD             
*******************************************************                         
MYGETEL  NTR1                                                                   
         SR    R2,R2                                                            
         ICM   R2,15,4(R1)                                                      
         BNZ   MG100                                                            
         ICM   R2,7,1(R1)                                                       
         LA    R2,27(R2)                                                        
         B     MG120                                                            
*                                                                               
MG100    ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
MG120    CLI   0(R2),0                                                          
         BE    MGXIT                                                            
         CLC   0(1,R1),0(R2)       ELEMENT EQUAL                                
         BNE   MG100                                                            
         ST    R2,4(R1)            RETURN ADDRESS                               
         LTR   R2,R2                                                            
MGXIT    XIT1                                                                   
         DROP  R3                                                               
*******************************************************                         
         EJECT                                                                  
*******************************************************                         
* TOTALS - PRINT TOTAL LINE AT END                                              
*                                                                               
TOTALS   NTR1                                                                   
         L     R6,ASPOOLA                                                       
         USING SPOOLD,R6                                                        
         LA    R5,P1                                                            
         USING COLINFO,R5                                                       
         MVC   CLLEN(14),=C'*** TOTALS ***'                                     
*                                                                               
         CLC   TOTCOST,=XL4'3B9AC9FF'    > $10M                                 
         BH    TOT5                                                             
         EDIT  TOTCOST,(10,CLACT),2,FLOAT=-                                     
         B     TOT10                                                            
TOT5     EDIT  TOTCOST,(10,TEMPCOST),FLOAT=-                                    
         MVC   CLACT+2(8),TEMPCOST                                              
*                                                                               
TOT10    CLC   TOTCLEAR,=XL4'3B9AC9FF'    > $10M                                
         BH    TOT15                                                            
         EDIT  TOTCLEAR,(10,CLCLR),2,FLOAT=-                                    
         B     TOT20                                                            
TOT15    EDIT  TOTCLEAR,(10,TEMPCOST),FLOAT=-                                   
         MVC   CLCLR+2(8),TEMPCOST                                              
*                                                                               
TOT20    CLC   TOTCLRN,=XL4'3B9AC9FF'    > $10M                                 
         BH    TOT25                                                            
         EDIT  TOTCLRN,(10,CLCLRN),2,FLOAT=-                                    
         B     TOT30                                                            
TOT25    EDIT  TOTCLRN,(10,TEMPCOST),FLOAT=-                                    
         MVC   CLCLRN+2(8),TEMPCOST                                             
*                                                                               
TOT30    CLC   TOTG,=XL4'3B9AC9FF'    > $10M                                    
         BH    TOT35                                                            
         EDIT  TOTG,(10,CLPAYGR),2,FLOAT=-                                      
         B     TOT40                                                            
TOT35    EDIT  TOTG,(10,TEMPCOST),FLOAT=-                                       
         MVC   CLPAYGR+2(8),TEMPCOST                                            
*                                                                               
TOT40    CLC   TOTN,=XL4'3B9AC9FF'    > $10M                                    
         BH    TOT45                                                            
         EDIT  TOTN,(10,CLPAYNT),2,FLOAT=-                                      
         B     TOTX                                                             
TOT45    EDIT  TOTN,(10,TEMPCOST),FLOAT=-                                       
         MVC   CLPAYNT+2(8),TEMPCOST                                            
*                                                                               
TOTX     DS    0H                                                               
         GOTO1 VSPOOL,DMCB,(R6)    PRINT IT                                     
         XIT1                                                                   
         DROP  R6                                                               
         DROP  R5                                                               
*******************************************************                         
         EJECT                                                                  
*******************************************************                         
* INITSPUL - INITIALIZE PRINT QUEUE AND SPOOL                                   
* LOCALS R6 - SPOOLD                                                            
*******************************************************                         
INITSPUL NTR1                                                                   
         L     R6,ASPOOLA                                                       
         USING SPOOLD,R6                                                        
         XC    SPOOLKEY,SPOOLKEY                                                
         LA    R2,SPOOLKEY         KEY FOR PRINT Q                              
         USING PQPLD,R2                                                         
         MVC   PLDESC,SPACES                                                    
         MVC   PLDESC(4),RCPROG                                                 
         MVC   PLSUBID,SPOOLID                                                  
         MVC   SPOOLDM,VDATMGR                                                  
         MVC   RCDATCON,VDATCON                                                 
         MVC   RCCOMFAC,ACOMFACS                                                
         MVC   SPOOLBUF,ATIA                                                    
         MVI   SPMODE,0            INITIALIZE SPOOL DSECT                       
         GOTO1 VDATCON,DMCB,(5,0),(10,RCDATE)  TODAYS DATE                      
         GOTO1 VSPOOL,DMCB,(R6)                                                 
         MVC   SPOOLRPN,PLREPNO                                                 
         XIT1                                                                   
         DROP  R6                                                               
*******************************************************                         
         EJECT                                                                  
*******************************************************                         
* INITREP  - INITIALIZE SPOOL FOR PAYLIST REPORT                                
* INPUT  - NBSEQ - BLASNK OR C'D' FOR DATE SEQ, C'P' OR 'Q' FOR PROG            
* OUTPUT - DATOFF,PRGOFF - OFFSET TO PRINT PROG GROUP AND DATE GROUP,           
*                          BASED ON NBSEQ                                       
* LOCALS R6 - SPOOLD                                                            
*******************************************************                         
INITREP  NTR1                                                                   
         L     R6,ASPOOLA                                                       
         USING SPOOLD,R6                                                        
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,SSPEC1           SSPECS FOR HEADER                            
         ST    R1,SPECS                                                         
*                                                                               
         CLI   NBSEQ,C'P'          CK IF PROG SEQ                               
         BE    PROGSEQ                                                          
         CLI   NBSEQ,C'Q'                                                       
         BE    PROGSEQ                                                          
DATSEQ   XC    DATOFF,DATOFF       DATE SEQ: DATE 1ST, PROGS AFTER              
         LA    R1,DATGRPL                                                       
         ST    R1,PRGOFF                                                        
         B     IRXIT                                                            
PROGSEQ  XC    PRGOFF,PRGOFF       PROG SEQ: PRGS 1ST, DATES AFTER              
         LA    R1,PRGGRPL                                                       
         ST    R1,DATOFF                                                        
IRXIT    XIT1                                                                   
         DROP  R6                                                               
*******************************************************                         
         EJECT                                                                  
*******************************************************                         
TESTREP  NTR1                                                                   
         TM    SPECREP,X'80'       TEST FOR ANY SPEC REP FILTER                 
         BO    *+14                YES                                          
         CLC   SPECREP,NBSREP      TEST FOR MATCH ON SPECIAL REP                
         BE    TREPFND             YES                                          
*                                                                               
         OC    NBSREP,NBSREP       TEST FOR ANY SPECIAL REP                     
         BNZ   TREPFND             YES GOOD PASS                                
*--CLEAR OUT TIME AND INTEGRATION NUMBERS                                       
         XC    TIMEPAYG(16),TIMEPAYG                                            
         XC    NBACTUAL(8),NBACTUAL                                             
         XC    NBPAYTGR(16),NBPAYTGR                                            
*--CHECK SPECIAL RATE FOR REP MATCH                                             
         SR    R5,R5                                                            
         USING NUSPRD,R5                                                        
TREP050  BAS   RE,GETSPRAT         GET SPECIAL RATE ELEMENT                     
         CLI   DMCB+12,X'FF'       GOOD RETURN CODE                             
         BE    TREPPAS             NO-SKIP RECORD                               
         L     R5,DMCB+12                                                       
*                                                                               
         TM    SPECREP,X'80'       TEST FOR ANY SPEC REP FILTER                 
         BO    *+18                YES                                          
         CLC   SRATREP,NUSPRREP    TEST FOR MATCH ON SPECIAL REP                
         BNE   TREP050             NO READ NEXT ELEM                            
         B     TREPFND             YES                                          
*                                                                               
         CLC   1(3,RE),=XL3'404040'   TEST FOR ANY SPECIAL REP                  
         BE    TREP050             READ NEXT ELEM                               
         OC    1(3,RE),1(RE)                                                    
         BZ    TREP050             READ NEXT ELEM                               
         B     TREPFND             GOOD RECORD                                  
*                                                                               
TREPFND  MVI   DMCB,0                                                           
         B     TREPEXIT                                                         
*                                                                               
TREPPAS  MVI   DMCB,X'FF'                                                       
         B     TREPEXIT                                                         
*                                                                               
TREPEXIT SR    R5,R5                                                            
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
*******************************************************                         
         EJECT                                                                  
*******************************************************                         
* HOOK - HEADLINES                                                              
* LOCALS - R6 - SPOOLD                                                          
*          R5 - CURRENT PRINT LINE                                              
*                                                                               
HOOK     NTR1                                                                   
         L     R6,ASPOOLA                                                       
         USING SPOOLD,R6                                                        
         LA    R5,HEAD1                                                         
         USING PHEAD,R5                                                         
         MVC   PHCLI,PAYCLI                                                     
         MVC   PHCLNM,PAYCLIN                                                   
         MVC   PHPRD,NBSELPRD                                                   
         MVC   PHPRNM,PRDNAME                                                   
         MVC   PHNET,PAYNET                                                     
         CLI   PAYEEH+5,0                                                       
         BE    HK30                                                             
         MVC   PHPAYEE(7),=CL7'PAYEE -'                                         
         TM    SPECREP,X'80'                                                    
         BZ    *+14                                                             
         MVC   PHSREP,=CL3'ALL'                                                 
         B     HK30                                                             
         MVC   PHSREP,PAYEE+1                                                   
*                                                                               
HK30     MVC   PHEST,=C'ALL'       IF ALL ESTS                                  
         TM    ESTTYP,IALL                                                      
         BNZ   HK70                                                             
         OC    EST,EST             ALSO MAY BE ALL IF EST=0                     
         BZ    HK70                                                             
         MVC   PHEST,EST           IF EST FILTER, FILTER IN EST                 
         TM    ESTTYP,IFILTER                                                   
         BNZ   HK70                                                             
         EDIT  (1,EST),(3,PHEST)   IF SINGLE EST OR RANGE                       
         TM    ESTTYP,ISINGLE                                                   
         BNZ   ISSING                                                           
         TM    ESTTYP,IRANGE       MUST BE RANGE                                
         BNZ   HK50                                                             
         DC    H'0'                                                             
HK50     MVI   PHEST+3,C','        FOR RANGE                                    
         EDIT  (1,EST+1),(3,PHEST+4)                                            
         B     HK70                                                             
ISSING   MVC   PHESNM,ENAME                                                     
HK70     OC    FILTER,FILTER       IF FILTER EXISTS                             
         BZ    HK90                                                             
         MVC   PHFLTHED(9),=C'FILTERS -'                                        
         GOTO1 VDISFIL             PUTS FILTER IN FLD                           
         MVC   PHFILT,FLD                                                       
         DROP  R5                                                               
*                                                                               
*** NOW DO HEADINGS                                                             
*** USE R4 TO POINT TO PROG GROUP LOC, AND R3 POINTS TO DATE GROUP              
*** PRGOFF AND DATOFF ARE PASSED IN                                             
*                                                                               
         USING PRGGRP,R4                                                        
         USING DATGRP,R3                                                        
         USING COLINFO,R5                                                       
HK90     LA    R5,H9                                                            
         LR    R4,R5                                                            
         A     R4,PRGOFF                                                        
         LR    R3,R5                                                            
         A     R3,DATOFF                                                        
*                                                                               
         MVC   CLDATE(4),=C'DATE'                                               
         MVC   CLPRTM(4),=C'TIME'                                               
         MVC   CLDAY(3),=C'DAY'                                                 
         MVC   CLPRNM(12),=C'PROGRAM NAME'                                      
         MVC   CLPRCD(16),=C'PROGRAM/EST/PACK'                                  
         MVC   CLPRD1(7),=C'PRODUCT'                                            
         MVC   CLLEN(3),=C'LEN'                                                 
         MVC   CLDAYP(2),=C'DP'                                                 
         MVC   CLB(1),=C'B'                                                     
         MVC   CLCTIME+5(4),=C'UNIT'                                            
         MVC   CLINT+2(9),=C'COST DATA'                                         
         MVC   CLCLR+1(7),=C'CLEARED'                                           
         MVC   CLCLR+132+2(5),=C'GROSS'                                         
         MVC   CLCLRN+1(7),=C'CLEARED'                                          
         MVC   CLCLRN+132+3(3),=C'NET'                                          
         MVC   CLPAYGR(7),=C'PAYABLE'                                           
         MVC   CLPAYGR+132(5),=C'GROSS'                                         
         MVC   CLPAYNT(7),=C'PAYABLE'                                           
         MVC   CLPAYNT+132+2(3),=C'NET'                                         
         MVC   CLFILT(7),=C'FILTERS'                                            
*                                                                               
         CLI   PAYDLRS,0           IS COMMERCIAL DISPLAYED                      
         BE    *+10                                                             
         MVC   CLCOMML(10),=C'COMMERCIAL'                                       
*                                                                               
         LA    R5,H11              DO UNDERLINES                                
         LR    R4,R5                                                            
         A     R4,PRGOFF                                                        
         LR    R3,R5                                                            
         A     R3,DATOFF                                                        
         MVC   CLDATE(6),UNDERS                                                 
         MVC   CLPRTM(9),UNDERS                                                 
         MVC   CLPRNM(16),UNDERS                                                
         MVC   CLLEN(7),UNDERS                                                  
         MVC   CLDAYP+3(1),UNDERS                                               
         MVC   CLCTIME(16),UNDERS                                               
         MVC   CLCLR(10),UNDERS                                                 
         MVC   CLCLRN(10),UNDERS                                                
         MVC   CLPAYGR(10),UNDERS                                               
         MVC   CLPAYNT(10),UNDERS                                               
         MVC   CLFILT(10),UNDERS                                                
*                                                                               
         CLI   PAYDLRS,0           IS COMMERCIAL DISPLAYED                      
         BE    *+10                                                             
         MVC   CLCOMML(10),UNDERS                                               
         XIT1                                                                   
UNDERS   DC    50CL1'-'                                                         
         DROP  R6                                                               
         DROP  R5                                                               
         DROP  R4                                                               
         DROP  R3                                                               
*******************************************************                         
         EJECT                                                                  
* ERROR EXIT                                                                    
*                                                                               
ERROR    GOTO1 VERROR                                                           
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
         DS    0H                                                               
PATCH    DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
NODATA   DC    C'** NO UNITS FOUND - CHECK KEY FIELDS/OPTIONS **'               
INVREQ   DC    C'** ### NOT A VALID REQUESTOR NAME **'                          
*                                                                               
*** SSPECS                                                                      
SSPEC1   DS    0H                  SSPECS FOR REPORT                            
*                                                                               
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H1,45,C'NETWORK PAYING LIST'                                     
         SSPEC H1,76,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,45,C'-------------------'                                     
         SSPEC H2,76,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,41,PERIOD                                                     
         SSPEC H4,76,NETREP                                                     
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H5,41,C'NETWORK - '                                              
         SSPEC H5,90,PAGE                                                       
         SSPEC H6,1,C'ESTIMATE'                                                 
*                                                                               
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
*******************************************************                         
         EJECT                                                                  
*******************************************************                         
* ENDSPOOL - CLOSE PRINT QUEUE. SHOW REPORT ID.                                 
* LOCALS R6 - SPOOLD                                                            
*******************************************************                         
ENDSPOOL NTR1  BASE=*,LABEL=*                                                   
         L     R6,ASPOOLA                                                       
         USING SPOOLD,R6                                                        
         MVI   SPMODE,X'FF'        CLOSE PRINT QUEUE                            
         GOTO1 VSPOOL,DMCB,(R6)                                                 
         OC    SPOOLPAG,SPOOLPAG   DIE IF NO REPORT                             
         BNZ   ES2                                                              
         DC    H'0'                                                             
ES2      LA    R3,PAYMSG                                                        
         MVC   PAYMSG(EMSGLEN),ENDMESSG      HEAD LINE OF REPORT                
         EDIT  (2,SPOOLPAG),(4,43(R3)),ALIGN=LEFT                               
         EDIT  (2,SPOOLLIN),(5,54(R3)),ALIGN=LEFT                               
         MVC   PAYMSG+28(3),SPOOLID                                             
         EDIT  (2,SPOOLRPN),(5,32(R3)),ALIGN=LEFT                               
         GOTO1 VSQUASH,DMCB,PAYMSG,L'PAYMSG                                     
*                                                                               
         MVC   PAYACT(3),SPOOLID   PUT ID IN ACTION FIELD                       
         MVI   PAYACT+3,C','       FOR QUICK $DIP                               
         LA    R2,PAYACT+4                                                      
         EDIT  (2,SPOOLRPN),(5,(R2)),ALIGN=LEFT                                 
         OI    PAYACTH+6,X'80'     DISPLAY IT                                   
*                                                                               
         XIT1                                                                   
         DROP  R6                                                               
ENDMESSG DC    C'REPORT HAS BEEN SPOOLED. '                                     
         DC    C'ID=XYZ,1234 PAGES=NNNN LINES=NNNN'                             
EMSGLEN  EQU   *-ENDMESSG                                                       
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEPAYWRK                                                       
         SPACE 2                                                                
         EJECT                                                                  
       ++INCLUDE FAPQPL                                                         
       ++INCLUDE NETBILLRD                                                      
         EJECT                                                                  
         DSECT                                                                  
PHEAD    EQU   *                                                                
PHLENGTH EQU   132                                                              
*                                                                               
         ORG   PHEAD+3*PHLENGTH+10                                              
PHCLI    DS    CL3                 CLIENT ABBR                                  
         DS    CL1                                                              
PHCLNM   DS    CL20                CLIENT NAME                                  
         DS    CL5                                                              
         ORG   PHEAD+4*PHLENGTH+10                                              
PHPRD    DS    CL3                 PRODUCT ABBR                                 
         DS    CL1                                                              
PHPRNM   DS    CL20                PRODUCT NAME                                 
         ORG   PHEAD+4*PHLENGTH+50                                              
PHNET    DS    CL4                 NETWORK                                      
         ORG   PHEAD+5*PHLENGTH+10                                              
PHEST    DS    CL3                 ESTIMATE ABBR                                
         DS    CL1                                                              
PHESNM   DS    CL(L'ENAME)         ESTIMATE NAME                                
         DS    CL1                                                              
         ORG   PHEAD+5*PHLENGTH+40                                              
PHPAYEE  DS    CL7                 PAYEE LITERAL                                
         DS    CL1                                                              
PHSREP   DS    CL3                 PAYEE                                        
         ORG   PHEAD+5*PHLENGTH+76                                              
PHFLTHED DS    CL10                C'FILTERS - ' IF ONE GIVEN                   
PHFILT   DS    CL6                 FILTERS                                      
*                                                                               
*DSECT FOR PRINT LINES                                                          
DATGRP   DSECT                     DATE GROUP                                   
DATBEG   EQU   *                                                                
CLDATE   DS    CL5                 DATE                                         
CLSUB    DS    CL3                 SUB-LINE PRINT                               
         DS    CL1                                                              
CLPRTM   DS    CL11                PROGRAM TIME                                 
         DS    CL1                                                              
DATGRPL  EQU   *-DATBEG            LENGTH OF DATE GROUP                         
         ORG   CLPRTM+132                                                       
CLDAY    DS    CL3                 DAY NAME                                     
*                                                                               
PRGGRP   DSECT                     PROGRAM GROUP                                
PRGBEG   EQU   *                                                                
CLPRNM   DS    CL16                PROGRAM NAME                                 
         DS    CL1                                                              
PRGGRPL  EQU   *-PRGBEG            LENGTH OF PROGRAM GROUP                      
         ORG   CLPRNM+132                                                       
CLPRCD   DS    CL6                 PROGRAM CODE                                 
CLSLSH1  DS    CL1                 SLASH                                        
CLEST    DS    CL3                 ESTIMATE                                     
CLSLSH2  DS    CL1                 SLASH                                        
CLPAK    DS    CL3                 PACKAGE                                      
*                                                                               
*********** MAINLINE STUFF                                                      
         DSECT                                                                  
COLINFO  EQU   *                                                                
*                                                                               
         DS    CL(DATGRPL+PRGGRPL)  GAP FOR DATE AND PROG INFO                  
CLLEN    DS    CL3                 LENGTH (SECONDS)                             
         DS    CL2                                                              
CLDAYP   DS    CL2                                                              
         DS    CL3                                                              
CLCTIME  DS    CL5                 C'TIME'                                      
         DS    CL1                                                              
CLACT    DS    CL10                ACTUAL COST                                  
         DS    CL1                                                              
CLCLR    DS    CL10                GROSS CLEARED                                
         DS    CL1                                                              
CLCLRN   DS    CL10                NET CLEARED                                  
         DS    CL1                                                              
CLPAYGR  DS    CL10                GROSS PAYABLE                                
         DS    CL1                                                              
CLPAYNT  DS    CL10                NET PAYABLE                                  
         DS    CL1                                                              
CLFILT   DS    CL6                                                              
         DS    CL5                                                              
CLCOMML  DS    CL8                                                              
*                                                                               
         ORG   CLLEN+132                                                        
CLPRD1   DS    CL3                 PRODUCT1                                     
CLSLASH  DS    CL1                 SLASH (/) IF PRD2 EXISTS                     
CLPRD2   DS    CL3                 PRODUCT2                                     
         DS    CL1                                                              
CLB      DS    CL1                 * IF BILLED                                  
         DS    CL1                                                              
         ORG   CLCTIME+132                                                      
CLINT    DS    CL5                 C'INT'                                       
         DS    CL1                                                              
CLICOS   DS    CL10                INTEG COST                                   
         DS    CL1                                                              
CLICLR   DS    CL10                INTEG GROSS CLEARED                          
         DS    CL1                                                              
CLICLRN  DS    CL10                INTEG NET CLEARED                            
         DS    CL1                                                              
CLIPAYGR DS    CL10                INTEG GROSS PAYABLE                          
         DS    CL1                                                              
CLIPAYNT DS    CL10                INTEG NET PAYABLE                            
*                                                                               
         ORG   CLFILT+132                                                       
CLSREP   DS    CL9                 C'SPEC REP='                                 
CLREP    DS    CL3                                                              
*                                                                               
         ORG   COLINFO                                                          
         DS    CL(DATGRPL)                                                      
         DS    CL9                                                              
CLSPRDS  DS    CL15                                                             
         ORG   CLCTIME                                                          
CLSRA    DS    CL5                 C'SRATE'                                     
         DS    CL1                                                              
CLSACT   DS    CL10                SPECIAL-RATE COST                            
         DS    CL1                                                              
CLSCLR   DS    CL10                SPECIAL-RATE GROSS CLEARED                   
         DS    CL1                                                              
CLSCLRN  DS    CL10                SPECIAL-RATE NET CLEARED                     
         DS    CL1                                                              
CLSPAYGR DS    CL10                SPECIAL-RATE GROSS PAYABLE                   
         DS    CL1                                                              
CLSPAYNT DS    CL10                SPECIAL-RATE NET PAYABLE                     
*                                                                               
         ORG   CLFILT                                                           
CLSRSREP DS    CL9                 C'SPEC REP='                                 
CLSRREP  DS    CL3                                                              
*                                                                               
         ORG   COLINFO                                                          
         DS    CL(DATGRPL)                                                      
         DS    CL9                                                              
CLTPRDS  DS    CL15                                                             
         ORG   CLCTIME                                                          
CLTOT    DS    CL5                 C'TOT'                                       
         DS    CL1                                                              
CLTACT   DS    CL10                TOTAL COST                                   
         DS    CL1                                                              
CLTCLR   DS    CL10                TOTAL GROSS CLEARED                          
         DS    CL1                                                              
CLTCLRN  DS    CL10                TOTAL NET CLEARED                            
         DS    CL1                                                              
CLTPAYGR DS    CL10                TOTAL GROSS PAYABLE                          
         DS    CL1                                                              
CLTPAYNT DS    CL10                TOTAL NET PAYABLE                            
*                                                                               
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
USERPAY  DS    C                                                                
OUTMSG   DS    CL80                                                             
SAVUTOT  DS    F                   SAVE UNIT TOTAL COST                         
*                                                                               
TEMPWORK DS    XL18                                                             
*                                                                               
SRATCOST DS    F                   SPECIAL RATE UNIT COST                       
SRATNET  DS    F                   SPECIAL RATE NET COST                        
SRATCLRG DS    F                   SPECIAL RATE CLEARED GROSS                   
SRATCLRN DS    F                   SPECAIL RATE CLEARED NET                     
SRATPAYG DS    F                   SPECIAL RATE PAYABLE GROSS                   
SRATPAYN DS    F                   SPECIAL RATE PAYABLE NET                     
*                                                                               
UNITCOST DS    F                   UNIT COST                                    
UNITCLRG DS    F                   UNIT CLEARED GROSS                           
UNITCLRN DS    F                   UNIT CLEARED NET                             
*                                                                               
TIMEPAYG DS    F                   TIME PAYABLE GROSS                           
TIMEPAYN DS    F                   TIME PAYABLE NET                             
*                                                                               
INTGPAYG DS    F                   INTEGRATION PAYABLE GROSS                    
INTGPAYN DS    F                   INTEGRATION PAYABLE NET                      
*                                                                               
PRGOFF   DS    F                   OFFSET OF PROG GROUP                         
DATOFF   DS    F                   OFFSET OF DATE GROUP                         
         DS    0D                                                               
SVPAYST  DS    CL1                 SAVED PAY STATUS                             
RECSW    DS    CL1                 FLAG SET IF ANY RECORDS                      
DISPRD   DS    XL1                 PRODUCT NUMBER TO PRINT                      
PRDLINE  DS    CL24                PRODUCT OUTPUT FORMAT                        
BLOCK    DS    CL256                                                            
TOTACT   DS    F                   TOTAL ACTUAL $                               
TOTINT   DS    F                   TOTAL INTEGRATION $                          
TOTSRAT  DS    F                   TOTAL TOTAL SPECIAL RATE $                   
TOTCOST  DS    F                   TOTAL COST (ACTUAL + INTEGRATION)            
TOTCLEAR DS    F                   TOTAL CLEARED $ (GROSS)                      
TOTCLRN  DS    F                   TOTAL CLEARED $ (NET)                        
*                                                                               
TEMPCOST DS    XL10                                                             
*                                                                               
PRODTBL  DS    CL18                PROD TABLE FOR NETVALUE                      
*                                                                               
SPECBIT  DS    XL2                 SPECIAL REP BACKUIP STATUS BIT               
SPECBREP DS    XL3                 SPECIAL REP CODE BACKUP                      
SRATCODE DS    XL1                 SPECIAL RATE CODE                            
SRATTAB  DS    XL512               SPECIAL RATE TABLE                           
*                                  BYTE 0-0   RATE CODE                         
*                                  BYTE 1-3   REP CODE                          
*                                  BYTE 4-7   UNIT COST GROSS                   
*                                  BYTE 8-11  UNIT COST NET                     
*                                  BYTE 12-15 CLEARED GROSS                     
*                                  BYTE 16-19 CLEARED NET                       
*                                  BYTE 20-23 PAYABLE GROSS                     
*                                  BYTE 24-27 PAYABLE NET                       
*                                  BYTE 28-31 RAW GROSS COST                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'122NEPAY04   11/20/17'                                      
         END                                                                    
