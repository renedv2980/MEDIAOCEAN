*          DATA SET ACCLB56    AT LEVEL 111 AS OF 08/16/00                      
*PHASE T62156A                                                                  
CLB56    TITLE '- PC COMMS - INTERNAL INVOICES'                                 
CLB56    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB56**,R8,RR=RE                                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         ST    RE,BORELO                                                        
         L     RC,ALINK                                                         
         USING LINKD,RC                                                         
*                                                                               
         CLI   LINKMODE,ROUTSN                                                  
         BNL   EXITY                                                            
         XR    RF,RF                                                            
         IC    RF,LINKMODE                                                      
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     SETMAP              SET A(MAP TABLE)                             
         B     RCVFST              FIRST FOR RECEIVE                            
         B     RCVHDRF             FIRST FOR MAP HEADER RECEIVE                 
         B     RCVDATA             DATA RECEIVE                                 
         B     RCVHDRL             LAST FOR MAP HEADER RECEIVE                  
         B     RCVLST              LAST FOR RECEIVE                             
         B     SND                 SEND                                         
ROUTSN   EQU   (*-ROUTS)/L'ROUTS                                                
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITY    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* SET A(MAP TABLE)                                                    *         
***********************************************************************         
         SPACE 1                                                                
SETMAP   DS    0H                                                               
         LA    RF,MAPTAB           SET A(MAP TABLE)                             
         ST    RF,AMAPTAB                                                       
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR RECEIVE                                                   *         
***********************************************************************         
         SPACE 1                                                                
RCVFST   DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR MAP HEADER RECEIVE                                        *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRF  DS    0H                                                               
         MVI   IICOMYN,0                                                        
         MVI   IIALLYN,0                                                        
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* ELEMENT DATA RECEIVE                                                *         
***********************************************************************         
         SPACE 1                                                                
RCVDATA  DS    0H                                                               
         ICM   RF,15,ORCVDATA                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
         SPACE 1                                                                
RCVJOB   MVC   THISJOB,DATA      FOR DLOAD OF LIST OR UPLOAD OF AN ADD          
         B     EXITY                                                            
RCVDA    MVC   IIDA,DATA         DISK ADDRESS FOR UPLOAD OF CHANGE              
         B     EXITY                                                            
RCVWC    MVC   IIWC,DATA         WORKCODE                                       
         B     EXITY                                                            
RCVCNTRA MVC   IICNTRA,DATA      CONTRA A/C                                     
         B     EXITY                                                            
RCVREF   MVC   IIREF,DATA        REFERENCE                                      
         B     EXITY                                                            
RCVDATE  MVC   IIDATE,DATA       DATE                                           
         B     EXITY                                                            
RCVNARR  MVC   IINARR,DATA       NARRATIVE                                      
         MVC   IINARRL,DATALEN+1                                                
         B     EXITY                                                            
RCVCOMYN MVC   IICOMYN,DATA      COMMISIONABLE STATUS                           
         B     EXITY                                                            
RCVALLYN MVC   IIALLYN,DATA      ALLOCATE STATUS                                
         B     EXITY                                                            
***********************************************************************         
* RECEIVE INTERNAL INVOICE AMOUNT - FOR ADD                           *         
***********************************************************************         
         SPACE 1                                                                
RCVII    DS    0H                                                               
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING PRORATAD,LSPRATA                                                 
                                                                                
         OC    IIDA,IIDA           TEST ANY DA SENT, IF NOT MUST BE ADD         
         BNZ   RIICHA00                                                         
         MVC   FDEFCRUL,=C'SI'     SET DEFAULT CREDIT LEDGER TO SI              
         CLI   CUCTRY,CTRYGER                                                   
         BNE   *+10                                                             
         MVC   FDEFCRUL,=C'SK'     SET TO SK FOR GERMANY                        
                                                                                
         XC    IOKEY,IOKEY                                                      
         USING TRNRECD,IOKEY                                                    
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'ACTKUNT+L'ACTKLDG),BCCPYEL+(CPYPROD-CPYELD)            
         MVC   TRNKACT,THISJOB                                                  
         GOTO1 AIO,IOACCDIR+IOREAD                                              
         BNE   EXITN               JOB NOT FOUND                                
         MVC   JOBDA,TRNKDA                                                     
         GOTO1 ASETUP,BODMCB,IOKEY+3,0,0                                        
*                                                                               
         CLC   IIWC,=C'99'                                                      
         BE    *+14                                                             
         CLC   IIWC,=C'**'                                                      
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         B     EXITN                                                            
         GOTO1 AGETWRK,IIWC                                                     
         BNE   EXITN                                                            
         CLI   CUCTRY2,CTRYGER     TEST GERMANY                                 
         BNE   RIIVAL10                                                         
         CLI   IIWC,C'Z'           TEST INTERNAL W/C (1ST CHAR ALPHA)           
         BNH   RIIVAL10                                                         
         MVC   FVMSGNO,=AL2(AE$MUIWC)                                           
         B     EXITN                                                            
RIIVAL10 DS    0H                                                               
*                                                                               
         L     R1,AIO3             PASS JOB/WORKCODE IN IO3                     
         USING TRNRECD,R1                                                       
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'ACTKUNT+L'ACTKLDG),BCCPYEL+(CPYPROD-CPYELD)            
         MVC   TRNKACT,THISJOB                                                  
         MVC   TRNKWORK,IIWC       READ WORKCODE LEVEL OPTIONS                  
         DROP  R1                                                               
         GOTO1 AGETOPT,BODMCB,AIO3                                              
*                                                                               
         OC    IICNTRA,BCSPACES                                                 
         CLC   IICNTRA,BCSPACES    TEST CREDIT ACCOUNT PRESENT                  
         BH    RIIVAL20                                                         
         CLI   P#DINCAR,C' '       TEST DEFAULT INCOME ACCOUNT RULE             
         BNH   RIIVAL20                                                         
         CLI   P#DINCAR,C'N'       TEST NO DEFAULT                              
         BE    RIIVAL20                                                         
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
B        USING ACTRECD,BOWORK1                                                  
         MVC   B.ACTKEY,BCSPACES                                                
         MVC   B.ACTKCULA,GOINCAC-GOBBLOCK(RF)                                  
         CLI   P#DINCAR,C'B'       TEST LEDGER AS SET BY GETOPT                 
         BE    *+10                                                             
         MVC   B.ACTKLDG,P#DINCAR  SET LEDGER DIRECTLY FROM PROFILE             
         CLC   B.ACTKLDG,FDEFCRUL+L'ACTKLDG  TEST DEFAULT LEDGER                
         BNE   *+14                                                             
         MVC   IICNTRA,B.ACTKACT   EXTRACT JUST SPACE-PADDED ACCOUNT            
         B     *+14                                                             
         MVC   IICNTRA,B.ACTKCULA  EXTRACT U/L/ACCOUNT CODE                     
         MVI   IICNTRA,C'*'        INDICATE NOT DEFAULT SK LEDGER               
         DROP  B                                                                
RIIVAL20 CLC   IICNTRA,BCSPACES    CREDIT ACCOUNT - REQUIRED INPUT              
*        BNH   EXITN                                                            
I        USING ACTRECD,IOKEY                                                    
         MVC   I.ACTKEY,BCSPACES                                                
         MVC   I.ACTKCPY,CUABIN                                                 
         MVC   I.ACTKUNT(L'FDEFCRUL),FDEFCRUL                                   
         MVC   I.ACTKACT,IICNTRA                                                
         CLI   IICNTRA,C'*'         INDICATES LEDGER OVERRIDDEN                 
         BNE   RIIVAL30                                                         
         LA    R1,OVLDGS                                                        
         LA    R0,OVLDGSN                                                       
         CLC   0(OVLDGSL,R1),IICNTRA+1                                          
         BE    RIIVAL26                                                         
         LA    R1,OVLDGSL(R1)                                                   
         BCT   R0,*-14                                                          
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         B     EXITN                                                            
RIIVAL26 MVC   I.ACTKULA(L'IICNTRA-1),IICNTRA+1                                 
*                                                                               
RIIVAL30 GOTO1 AGETACT,0                                                        
         BNE   EXITN                                                            
         CLI   CUCTRY,CTRYGER                                                   
         BNE   RIIVAL40                                                         
         CLC   =C'SI',ACCODE+1     DISALLOW SI IN GERMANY                       
         BNE   RIIVAL40                                                         
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         B     EXITN                                                            
RIIVAL40 TM    ACBSTAT,ACBSABAL                                                 
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$WLACT)                                           
         B     EXITN                                                            
         TM    ACBSTAT,ACBSCLSE+ACBSLOCK                                        
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTCL)                                           
         B     EXITN                                                            
         MVC   IICNTRA,ACCODE+1                                                 
         MVC   IICNTRNA,ACNAME                                                  
         SPACE 1                                                                
***********************************************************************         
* ADD DRAFT TRANSACTION RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
         L     R4,AIO3             BUILD RECORD IN IO3                          
         USING TRNRECD,R4                                                       
         XC    TRNKEY,TRNKEY                                                    
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'TRNKUNT+L'TRNKLDG),BCCPYEL+(CPYPROD-CPYELD)            
         MVC   TRNKACT,THISJOB                                                  
         MVC   TRNKWORK,IIWC                                                    
         MVC   TRNKCCPY,CUABIN                                                  
         MVC   TRNKULC,IICNTRA     CONTRA IS CREDIT A/C                         
         MVC   TRNKDATE,IIDATE                                                  
         MVC   TRNKREF,IIREF                                                    
*                                                                               
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         MVI   TRNEL,TRNELQ                                                     
         MVC   TRNDATE,IIDATE                                                   
         MVC   TRNREF,IIREF                                                     
         MVI   TRNTYPE,169         ADD WITH SPECIAL TRANSACTION TYPE            
         MVI   TRNSUB,X'E0'        AND START WITH A HIGH SUB-REFERENCE          
         MVC   TRNMOS,BCTMON       SET TODAYS MOA                               
         MVC   TRNBREF,BCSPACES    SET NULL BATCH REFERENCE                     
         MVI   TRNSTAT,TRNSDR+TRNSAUTH                                          
         MVC   TRNANAL,IIWC                                                     
         SR    RE,RE                                                            
         ICM   RE,1,IINARRL                                                     
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TRNNARR(0),IINARR                                                
         LA    RE,TRNLN1Q+1(RE)                                                 
         STC   RE,TRNLN            SET ELEMENT LENGTH                           
         LA    RF,TRNEL(RE)                                                     
         MVI   0(RF),0             SET E-O-R                                    
         SR    RF,R4                                                            
         LA    RF,1(RF)                                                         
         STCM  RF,3,TRNRLEN        SET RECORD LENGTH                            
         ZAP   TRNAMNT,DATA(L'TRNAMNT)                                          
         BAS   RE,UPDTRN                                                        
         BNE   EXITN                                                            
*                                                                               
         L     R4,AADTBLK                                                       
         USING ADDTRND,R4                                                       
         GOTO1 AINIADT                                                          
         MVI   TRNINDS,TRNIDRFT+TRNICONV                                        
         MVC   TRNCACNM,IICNTRNA   CREDIT ACCOUNT NAME                          
         GOTO1 VADDTRN,ADDTRND                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRNINDS,TRNILAST                                                 
         GOTO1 VADDTRN,ADDTRND                                                  
         L     RE,AIO3                                                          
         XC    FIOKEY,FIOKEY                                                    
F        USING TRNRECD,FIOKEY                                                   
         MVC   F.TRNKEY,0(RE)                                                   
         MVC   F.TRNKSTA,TRNRSTA-TRNKEY(RE)                                     
         AH    RE,=Y(2000-4)                                                    
         MVC   IIDA,0(RE)          SAVE DISK ADDRESS OF TRANSACTION             
         MVC   F.TRNKDA,IIDA                                                    
         DROP  F                                                                
         GOTO1 ASETTRN,BOPARM,(C'S',FIOKEY),AIO3,LSPRATA                        
*                                                                               
         MVC   IODAOVER,JOBDA      GET JOB RECORD                               
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AUPDJOB,BOPARM,(C'U',AIO3),AIO1,0,LSPRATA                        
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                 NEED TO SEND BACK THE DA                     
         DC    H'0'                                                             
         XC    THISJOB,THISJOB                                                  
         B     EXITY                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* UPDATE TRANSACTION PTAEL                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R4                                                       
         USING TRNELD,R3                                                        
UPDTRN   NTR1                                                                   
*                                                                               
UPDT01   LA    R3,TRNRFST          R3=A(TRNEL)                                  
         OI    TRNSTAT,TRNSNOCM    ASSUME NON-COMMISSIONABLE                    
         CLI   IICOMYN,0                                                        
         BZ    *+8                                                              
         NI    TRNSTAT,FF-(TRNSNOCM)                                            
                                                                                
UPDT06   GOTO1 AGETOPT,BODMCB,AIO3                                              
         SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    UPDT07                                                           
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    UPDT07                                                           
         LA    R0,CSEXCVAL                                                      
UPDT07   GOTO1 APRORATA,BODMCB,TRNRECD,AGOPBLK,ACOM,(R0),LSPRATA,0              
         LA    RE,LSPRATAS                                                      
         LA    RF,PR$LNQ                                                        
         LA    R0,LSPRATA                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         ZAP   TRNAMNT,DATA(L'TRNAMNT)                                          
*                                                                               
UPD08    DS    0H                                                               
**                                                                              
UPDT30   NI    TRNRSTA2,FF-TRNSBILP  NO BILL ACTIVITY PENDING                   
         USING TRXELD,R1                                                        
         LA    R1,TRNRFST                                                       
         SR    R0,R0                                                            
UPDT32   IC    R0,TRXLN                                                         
         AR    R1,R0                                                            
         CLI   TRXEL,0                                                          
         BE    UPDT34                                                           
         CLI   TRXEL,TRXELQ                                                     
         BNE   UPDT32                                                           
         NI    TRXSTA2,FF-TRNSBILP                                              
*                                                                               
UPDT34   DS    0H                                                               
UPDT35   ZAP   BODUB1,BCPZERO      CLEAR ALLOCATION                             
         GOTO1 AALLTRN,BODMCB,('PTATRAL',AIO3),('PTASCASH',LSPRATA),   X        
               (1,BODUB1),0                                                     
         CLI   IIALLYN,0           TEST ALLOCATED                               
         BE    UPDT40                                                           
         ZAP   BODUB1,DATA(L'TRNAMNT)                                           
         GOTO1 AALLTRN,BODMCB,('PTATRAL',AIO3),('PTASCASH',LSPRATA),   X        
               (1,BODUB1),0                                                     
         BNE   EXITN                                                            
UPDT40   CLI   IIALLYN,0           TEST ALLOCATED                               
         BE    UPDT50                                                           
         CLI   IICOMYN,0           TEST COMMISSIONABLE                          
         BE    UPDT50                                                           
         L     RF,AGOPBLK          CALCULATE COMMISSION                         
         ZAP   BOPL81(16),DATA(L'TRNAMNT)                                       
         SRP   BOPL81(16),2,0                                                   
         MP    BOPL81(16),GOAGYCOM-GOBLOCKD(L'GOAGYCOM,RF)                      
         SRP   BOPL81(16),64-8,5                                                
         ZAP   BODUB1,BOPL81(16)                                                
         B     *+10                                                             
UPDT50   ZAP   BODUB1,BCPZERO                                                   
         GOTO1 AALLTRN,BODMCB,('PTATRAL',AIO3),('PTASCASH',LSPRATA),   X        
               0,(1,BODUB1)                                                     
         BNE   EXITN                                                            
         SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    UPDT72                                                           
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    UPDT72                                                           
         LA    R0,CSEXCVAL                                                      
UPDT72   GOTO1 APRORATA,BODMCB,TRNRECD,AGOPBLK,ACOM,(R0),LSPRATA,0              
         B     EXITY                                                            
*                                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* INTERNAL INVOICE CHANGE                                             *         
***********************************************************************         
                                                                                
RIICHA00 MVC   IODAOVER,IIDA                GET THE TRANSACTION RECORD          
         GOTO1 AIO,IO3+IOACCMST+IOGETRUP    ACCMST INTO IO3                     
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R1,AIO3                                                          
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(L'TRNKCULA),0(R1)                                          
         L     R1,=A(IO5+IOACCDIR+IOREAD)                                       
         GOTO1 AIO                          JOB RECORD INTO IO5                 
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=A(IO5+IOACCMST+IOGETRUP)                                     
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASETUP,BODMCB,IOKEY+3,0,0                                        
                                                                                
         XR    R0,R0                        CALL PRORATA                        
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BODMCB,AIO3,AGOPBLK,ACOM,(R0),LSPRATA,0                 
                                                                                
         LA    R0,LSPRATAS                  COPY LSPRATA TO LSPRATAS            
         L     R1,=A(PR$LNQ)                                                    
         LA    RE,LSPRATA                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         XC    IOKEY,IOKEY         GET TRANSACTION ACCDIR                       
         L     R1,AIO3                                                          
         MVC   IOKEY(TRNKEND),0(R1)                                             
         GOTO1 AIO,IOACCDIR+IORDUP                                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 ASETTRN,BOPARM,(C'S',IOKEY),AIO3,LSPRATA                         
                                                                                
         L     R3,AIO3                                                          
         USING TRNRECD,R3                                                       
         USING TRNELD,TRNRFST                                                   
         NI    TRNSTAT,FF-TRNSNOCM                                              
         CLI   IICOMYN,1           COMMISIONABLE ?                              
         BE    *+8                                                              
         OI    TRNSTAT,TRNSNOCM    NO                                           
         ZAP   TRNAMNT,DATA(L'TRNAMNT)                                          
         ZAP   BODUB1,BCPZERO      CLEAR ALLOCATION                             
         GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO3),LSPRATA,(1,BODUB1),     *        
               (1,BODUB1)                                                       
         BNE   EXITN                                                            
         CP    TRNAMNT,BCPZERO     IF ZERO THEN MUST BE DELETING                
         BNE   RIICHA10                                                         
         OI    TRNRSTAT,TRNSDELT                                                
         OI    IOKEY+TRNKSTAT-TRNRECD,TRNSDELT                                  
         GOTO1 AIO,IOACCDIR+IOWRITE                                             
         BE    RIIUPD                                                           
         DC    H'0'                                                             
                                                                                
RIICHA10 CLI   IIALLYN,0           TEST ALLOCATED=YES                           
         BE    RIICHA20                                                         
         ZAP   BODUB1,BCPZERO                                                   
         TM    TRNSTAT,TRNSNOCM                                                 
         BO    *+10                                                             
         ZAP   BODUB1,PA$NLCOM                                                  
         GOTO1 AALLTRN,BOPARM,('PTATRAL',AIO3),('PTASCASH',LSPRATA),   *        
               (1,PA$NET),(1,BODUB1)                                            
         BNE   EXITN                                                            
         DROP  R3                                                               
                                                                                
RIICHA20 CLI   IINARRL,0                   ANY NARRATIVE RECEIVED?              
         BE    RIIUPD                                                           
         L     R3,AIO3                                                          
         USING TRNRECD,R3                                                       
         XC    BOELEM,BOELEM                                                    
         LA    R4,BOELEM                                                        
         USING TRNELD,R4                                                        
         IC    RE,TRNRFST+(TRNLN-TRNELD)                                        
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TRNELD(0),TRNRFST                                                
                                                                                
         MVC   TRNNARR,IINARR                                                   
         XR    RF,RF                                                            
         IC    RF,IINARRL                                                       
         LA    RF,TRNLN1Q(RF)             SET ELEMENT LENGTH                    
         STC   RF,TRNLN                                                         
                                                                                
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('TRNELQ',TRNRECD),0                 
         MVI   TRNEL,1             ADD ELEMENT AT START OF RECORD               
         GOTO1 (RF),(R1),(C'P',ACCMST),TRNRECD,TRNELD                           
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRNRFST,TRNELQ                                                   
         DROP  R3,R4                                                            
                                                                                
***********************************************************************         
* TRANSACTION / JOB UPDATE                                            *         
***********************************************************************         
                                                                                
RIIUPD   GOTO1 AIO,IO3+IOACCMST+IOPUT      WRITE BACK TRANSACTION               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AUPDJOB,BOPARM,(C'U',AIO3),AIO5,LSPRATAS,LSPRATA                 
         L     R1,=A(IO5+IOACCMST+IOPUT)                                        
         GOTO1 AIO                        WRITE BACK JOB RECORD                 
         XC    THISJOB,THISJOB                                                  
         XC    IIDA,IIDA                                                        
         BE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R5 MM                                                            
***********************************************************************         
* LAST FOR MAP HEADER RECEIVE                                         *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRL  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR RECEIVE                                                    *         
***********************************************************************         
         SPACE 1                                                                
RCVLST   DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* SEND                                                                *         
***********************************************************************         
         SPACE 1                                                                
SND      DS    0H                                                               
         ICM   RF,15,OSND                                                       
         BNZR  RF                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SEND INTERNAL INVOICES                                              *         
***********************************************************************         
         SPACE 1                                                                
SNDII    DS    0H                                                               
         OC    IIDA,IIDA           IF DA THEN MUST BE SENDING AFTER ADD         
         BZ    LSTFRST                                                          
         CLC   THISJOB,BCSPACES                                                 
         BH    EXITY                                                            
         GOTO1 ASNDHDR,BODMCB,14                                                
         GOTO1 ASNDDATA,BODMCB,1,IIDA                                           
         B     EXITY                                                            
                                                                                
LSTFRST  LA    R2,IOKEY            INITIALISE KEY                               
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,THISJOB                                                  
                                                                                
GETFRST  LA    R1,IOACCDIR+IOHI+IO1  * GET FIRST RECORD *                       
         B     *+8                                                              
GETNEXT  LA    R1,IOACCDIR+IOSQ+IO1  * GET NEXT RECORD *                        
         GOTO1 AIO,(R1)                                                         
         BNE   EXITN                                                            
         CLC   TRNKCULA,IOKEYSAV+TRNKCULA-TRNKEY                                
         BNE   EXITY                                                            
         CLC   TRNKDATE,BCSPACES   NOT TRANSACTION RECORD                       
         BE    GETNEXT                                                          
                                                                                
         CLI   TRNKSTYP,169                                                     
         BNE   GETNEXT                                                          
*        GOTO1 ASETTRN,BOPARM,(C'D',TRNRECD)                                    
*        BNE   GETNEXT             FILTER DIRECTORY RECORD                      
                                                                                
         MVC   IIDA,TRNKDA                                                      
         MVC   IODAOVER,TRNKDA                                                  
         GOTO1 AIO,IOACCMST+IOGET+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*        GOTO1 ASETTRN,BOPARM,(C'M',AIO1)                                       
*        BNE   GETNEXT                                                          
                                                                                
         GOTO1 ASNDHDR,BODMCB,14                                                
         GOTO1 ASNDDATA,BODMCB,1,IIDA                                           
         GOTO1 (RF),(R1),2,(L'TRNKACT,TRNKACT)                                  
         GOTO1 (RF),(R1),3,TRNKWORK                                             
         GOTO1 (RF),(R1),4,(L'TRNKULC,TRNKULC)                                  
         GOTO1 (RF),(R1),5,TRNKDATE                                             
         GOTO1 (RF),(R1),6,TRNKREF                                              
         L     R3,AIO2                                                          
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         USING TRNELD,R3                                                        
         CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC     H'0'                                                            
         GOTO1 (RF),(R1),7,TRNAMNT                                              
         TM    TRNSTAT,TRNSNOCM                                                 
         BZ    SNDII10                                                          
         GOTO1 (RF),(R1),9                                                      
SNDII10  XR    R4,R4                                                            
         CLI   TRNLN,TRNLN1Q                                                    
         BE    SNDII20                                                          
         IC    R4,TRNLN                                                         
         SH    R4,=Y(TRNLN1Q)                                                   
         GOTO1 (RF),(R1),8,((R4),TRNNARR)                                       
                                                                                
         USING PTAELD,R3                                                        
SNDII20  CLI   PTAEL,0                                                          
         BE    GETNEXT                                                          
         CLI   PTAEL,PTAELQ                                                     
         BNE   SNDII30                                                          
         CLI   PTATYPE,PTATRAL                                                  
         BNE   SNDII30                                                          
         CP    PTANET,BCPZERO                                                   
         BE    SNDII30                                                          
         GOTO1 ASNDDATA,BODMCB,10                                               
         B     GETNEXT                                                          
SNDII30  IC    R4,PTALN                                                         
         AR    R3,R4                                                            
         B     SNDII20                                                          
         DROP  R2,R3                                                            
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
ACCMST   DC    C'ACCMST '                                                       
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* MAP TABLE                                                           *         
***********************************************************************         
         SPACE 1                                                                
MAPTAB   DS    0X                                                               
*                                  ** INTERNAL INVOICES **                      
IIEL     DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(MH#IIL)         ELEMENT CODE                                 
         DC    AL2(IIELX+1-IIEL)   DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(SNDII-CLB56)    SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(255)            MAPPING CODE                                 
         DC    CL5'JOB  '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKACT)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVJOB-CLB56)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'DA'             TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKDA)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVDA-CLB56)    RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'CPJ  '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVJOB-CLB56)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'WC'             TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKWORK)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVWC-CLB56)    RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE                                 
         DC    CL5'CONTR'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVCNTRA-CLB56) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE                                 
         DC    CL5'DATE'           TEXT IDENTIFIER                              
         DC    AL1(MDTDTQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKDATE)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVDATE-CLB56)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(06)             MAPPING CODE                                 
         DC    CL5'REF'            TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNKREF)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVREF-CLB56)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(07)             MAPPING CODE                                 
         DC    CL5'TAMNT'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'TRNAMNT)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVII-CLB56)    RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(08)             MAPPING CODE                                 
         DC    CL5'NARRA'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVNARR-CLB56)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(09)             MAPPING CODE                                 
         DC    CL5'COMM?'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVCOMYN-CLB56) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(10)             MAPPING CODE                                 
         DC    CL5'ALLO?'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVALLYN-CLB56) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
IIELX    DC    XL1'00'             END OF ELEMENT FIELDS                        
*                                                                               
MAPTABX  DC    X'00'               E-O-T                                        
*                                                                               
OVLDGS   DS    0X                  OVERRIDE CREDIT ACCOUNT LEDGERS              
         DC    C'SK'                                                            
OVLDGSL  EQU   *-OVLDGS                                                         
*                                  FURTHER OVERRIDE LEDGERS HERE                
         DC    C'SI'                                                            
OVLDGSN  EQU   (*-OVLDGS)/OVLDGSL                                               
         EJECT                                                                  
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
***********************************************************************         
* SAVED WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   OSVALS                                                           
         DS    (L'OSVALS-(*-OSVALS))X                                           
         EJECT                                                                  
* ACCLBLINK                                                                     
       ++INCLUDE ACCLBLINK                                                      
         SPACE 1                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
LINKD    DSECT                                                                  
         ORG   OVRWS                                                            
IIDA     DS    XL(L'TRNKDA)        DISK ADDRESS                                 
THISJOB  DS    XL(L'TRNKACT)       CLI/PRO/JOB                                  
IIWC     DS    XL(L'TRNKWORK)       WORKCODE                                    
IICNTRA  DS    XL(14)              CONTRA A/C                                   
IIREF    DS    XL(L'TRNKREF)       REFERENCE                                    
IIDATE   DS    XL(L'TRNKDATE)      DATE                                         
IICOMYN  DS    XL1                 COMMISIONABLE STATUS                         
IIALLYN  DS    XL1                 ALLOCATE STATUS                              
IINARR   DS    XL(L'DATA)          NARRATIVE                                    
IINARRL  DS    XL1                 NARRATIVE LENGTH                             
FDEFCRUL DS    CL2                 DEFALT CREDIT ACCOUNT LEDGER                 
IICNTRNA DS    CL36                CONTRA ACCOUNT NAME                          
JOBDA    DS    XL(L'TRNKDA)        JOB RECORD DISK ADDRESS                      
FIOKEY   DS    XL(L'IOKEY)                                                      
         DS    (L'OVRWS-(*-OVRWS))X                                             
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'111ACCLB56   08/16/00'                                      
         END                                                                    
