*          DATA SET ACCLB67    AT LEVEL 049 AS OF 08/16/00                      
*PHASE T62167A                                                                  
*&&      SET   NOP=N                                                            
***********************************************************************         
* UPDATE FIELD PANELS (BFMELDS WITH BFMPANEL SET)                     *         
*                                                                     *         
* NTRY: P1 = C'ADD' IF ADDING PANELS TO NEW RECORD                    *         
*          = C'UPDATE' IF UPDATING PANELS FOR EXISTING RECORD         *         
*       P2 = A(INPUT RECORD)                                          *         
*       P3 = A(OUTPUT RECORD)                                         *         
*       P4 = A(BILL HEADER RECORD)                                    *         
*                                                                     *         
* EXIT: P1 BYTE 0 = X'80' ON IF RECORD CONTAINED PANELS               *         
*                   X'40' ON IF OUTPUT CHANGED FROM INPUT             *         
***********************************************************************         
CLB67    TITLE '- PC COMMS - BUILD BILL PAGE PANELS'                            
CLB67    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL BPWORKL,**CB67**,R8,CLEAR=YES,RR=RE                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         USING BPWORKD,RC                                                       
         L     R7,ALINK                                                         
         USING LINKD,R7                                                         
         L     R6,AGOPBLK                                                       
         USING GOBLOCKD,R6                                                      
         ST    R1,BPAR1                                                         
         MVC   BPPARMS,0(R1)                                                    
         L     RF,BPAACT                                                        
         MVC   BPACT,0(RF)         SET ACTION                                   
         USING BLHELD,BEWBLH                                                    
         EJECT                                                                  
***********************************************************************         
* PROCESS INPUT RECORD                                                *         
***********************************************************************         
         SPACE 1                                                                
PRCPAN   DS    0H                                                               
         L     RE,BPAINP           COPY INPUT RECORD                            
         LH    RF,BEDRLEN-BEDRECD(RE)   (IN CASE = OUTPUT RECORD)               
         LA    R0,BPINP                                                         
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R2,BPINP                                                         
I        USING BEDRECD,R2          R2 = A(INPUT RECORD)                         
         L     R4,BPAOUT                                                        
O        USING BEDRECD,R4          R4 = A(OUTPUT RECORD)                        
*                                  INITIALISE OUTPUT RECORD                     
         XC    O.BEDRECD(BEDRFST+1-BEDRECD),O.BEDRECD                           
         MVC   O.BEDKEY,I.BEDKEY                                                
         MVC   O.BEDRSTA,I.BEDRSTA                                              
         MVC   O.BEDRLEN,=AL2(BEDRFST+1-BEDRECD)                                
*                                                                               
         LA    R3,I.BEDRFST                                                     
         USING BFMELD,R3                                                        
PPAN02   DS    0H                                                               
         CLI   BFMEL,0                                                          
         BE    PPAN10                                                           
         CLI   BFMEL,BFMELQ                                                     
         BNE   PPAN06                                                           
         CLI   BFMTYPE,BFMTDATA                                                 
         BNE   PPAN06                                                           
         CLC   BFMPANEL,BCSPACES                                                
         BNH   PPAN06                                                           
         OI    BPINDS,BPIPAN                                                    
*                                                                               
         CLI   BPACT,BPACTUPD      TEST UPDATING                                
         BNE   PPAN04                                                           
         LA    R4,FLDTAB                                                        
         USING FLDTABD,R4                                                       
PPAN03   CLI   FLDTABD,EOT         YES - TEST FIELD SHOULD BE UDATED            
         BE    PPAN06                                                           
         CLC   FLDCODE,BFMPANEL                                                 
         BE    *+12                                                             
         LA    R4,FLDTABL(R4)                                                   
         B     PPAN03                                                           
         TM    FLDINDS1,FLDIUPDT   NOT UPDATEABLE - COPY ORIGINAL               
         BZ    PPAN06                                                           
         DROP  R4                                                               
*                                                                               
PPAN04   GOTO1 PRCBFM,BFMELD                                                    
         B     PPAN08                                                           
*                                                                               
PPAN06   GOTO1 ADDEL,BOPARM,BFMELD                                              
*                                                                               
PPAN08   DS    0H                                                               
         XR    RF,RF                                                            
         IC    RF,BFMLN                                                         
         BXH   R3,RF,PPAN02                                                     
         DROP  R3                                                               
*                                                                               
PPAN10   DS    0H                                                               
*                                                                               
         L     R1,BPAR1                                                         
         MVC   0(L'BPINDS,R1),BPINDS                                            
         B     EXITY                                                            
         EJECT                                                                  
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
* PROCESS BFMELD                                                      *         
*                                                                     *         
* NTRY: R1 = A(BFMELD)                                                *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
PRCBFM   NTR1  ,                                                                
         LR    R4,R1                                                            
O        USING BFMELD,R3           R3 = A(OLD ELEMENT)                          
         LA    R4,BPELS                                                         
N        USING BFMELD,R4           R4 = A(NEW ELEMENT)                          
*                                                                               
         XR    RF,RF                                                            
         IC    RF,O.BFMLN                                                       
         EX    RF,*+4                                                           
         MVC   N.BFMELD(0),O.BFMELD                                             
         MVI   N.BFMLN,BFMLNQ                                                   
*                                                                               
         GOTO1 GETDATA,N.BFMELD                                                 
*                                                                               
         LH    R2,BPDATALN                                                      
         LTR   R2,R2                                                            
         BZ    PRCBFMC                                                          
         STCM  R2,3,N.BFMTLEN                                                   
         EX    R2,*+4                                                           
         MVC   N.BFMTEXT(0),BPDATA                                              
         CLI   BPACT,BPACTUPD      MAY RESET WIDTH IF UPDATING                  
         BNE   PBFM02                                                           
         CLM   R2,1,N.BFMWTH                                                    
         BNH   PBFM02                                                           
         STC   R2,N.BFMWTH                                                      
*                                                                               
PBFM02   XR    RF,RF                                                            
         IC    RF,N.BFMLN                                                       
         AR    RF,R2                                                            
         STC   RF,N.BFMLN                                                       
*                                                                               
         GOTO1 ADDEL,BOPARM,N.BFMELD                                            
         IC    RF,O.BFMLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   PRCBFMC                                                          
         CLC   O.BFMELD(0),N.BFMELD                                             
*                                                                               
         B     EXITY                                                            
*                                                                               
PRCBFMC  OI    BPINDS,BPIUPD       SET RECORD NEEDS UPDATING                    
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ADD ELEMENT TO RECORD                                               *         
*                                                                     *         
* NTRY: P1 = A(ELEMENT)                                               *         
* EXIT: P2 = A(ELEMENT ADDED TO RECORD)                               *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
ADDEL    NTR1  ,                                                                
         LR    R4,R1                                                            
         L     R3,0(R4)                                                         
         USING BFMELD,R3                                                        
*                                                                               
         L     R2,BPAOUT                                                        
         USING BEDRECD,R2                                                       
         XR    RF,RF               TEST RECORD WILL BE TOO BIG                  
         ICM   RF,3,BEDRLEN                                                     
         XR    RE,RE                                                            
         IC    RE,BFMLN                                                         
         AR    RF,RE                                                            
         CHI   RF,2000                                                          
         BL    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),BEDRECD,BFMELD,ADDEND                
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   4(4,R4),16(R1)                                                   
*                                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET OUTPUT DATA                                          *         
*                                                                     *         
* NTRY: P1 = A(BFMELD)                                                *         
* EXIT: BPDATALN = LENGTH OF DATA                                     *         
*       BPDATA = DATA                                                 *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
GETDATA  NTR1  ,                                                                
         MVC   BPDATA(L'BCSPACES),BCSPACES                                      
         MVC   BPDATALN,=AL2(L'BCSPACES)                                        
         LR    R3,R1                                                            
         USING BFMELD,R3                                                        
         LA    R2,FLDTAB                                                        
         USING FLDTABD,R2                                                       
GDATA02  CLI   FLDTABD,EOT                                                      
         BE    GETDATAX                                                         
         CLC   FLDCODE,BFMPANEL                                                 
         BE    GDATA04                                                          
         LA    R2,FLDTABL(R2)                                                   
         B     GDATA02                                                          
*                                                                               
GDATA04  BAS   RE,GETFLD                                                        
*                                                                               
         XR    RE,RE               REMOVE TRAILING SPACES                       
         ICM   RE,3,BPDATALN                                                    
         BZ    GETDATAX                                                         
         LA    RF,BPDATA-1(RE)                                                  
GDATA06  CLI   0(RF),C' '                                                       
         BH    GDATA08                                                          
         BCTR  RF,0                                                             
         BCT   RE,GDATA06                                                       
GDATA08  STH   RE,BPDATALN                                                      
*                                                                               
GETDATAX B     EXIT                                                             
*                                                                               
GETFLD   NTR1  ,                                                                
         XR    RF,RF                                                            
         ICM   RF,3,FLDROUT                                                     
         LA    RF,CLB67(RF)                                                     
         BR    RF                                                               
         POP   USING                                                            
*                                                                               
PG       DS    0H                  * PAGE NUMBER *                              
         LH    RF,=Y(LC@PAGE-TWAD)                                              
         A     RF,ATWA                                                          
         MVC   BPDATA(L'LC@PAGE),0(RF)                                          
         LA    RF,BPDATA+L'BPDATA-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(3,RF),=C'XXX'     "PAGE XXX"                                   
         B     EXIT                                                             
*                                                                               
AN       DS    0H                  * ORIGIN NAME *                              
         MVC   BPDATA(L'BCORGNAM),BCORGNAM                                      
         B     EXIT                                                             
*                                                                               
AA       DS    0H                  * ORIGIN ADDRESS *                           
         MVC   BPDATA(L'BCORGADD),BCORGADD                                      
         B     EXIT                                                             
*                                                                               
TD       DS    0H                  * TODAY'S DATE *                             
         GOTO1 VDATCON,BOPARM,(X'41',BCTODAYP),(17,BPDATA)                      
         B     EXIT                                                             
*                                                                               
CON      DS    0H                  * COMPANY NAME *                             
         L     R3,GOACOMP                                                       
         LA    R3,ACCORFST(R3)                                                  
         USING NAMELD,R3                                                        
         XR    RF,RF                                                            
CON02    CLI   NAMEL,0                                                          
         BE    EXIT                                                             
         IC    RF,NAMLN                                                         
         CLI   NAMEL,NAMELQ                                                     
         BE    *+8                                                              
         BXH   R3,RF,CON02                                                      
         SH    RF,=Y(NAMLN1Q+1)                                                 
         BM    EXIT                                                             
         EX    RF,*+4                                                           
         MVC   BPDATA(0),NAMEREC                                                
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
COA      DS    0H                  * COMPANY ADDRESS *                          
         L     R3,GOACOMP                                                       
         LA    R3,ACCORFST(R3)                                                  
         USING ADRELD,R3                                                        
         XR    RF,RF                                                            
COA02    CLI   ADREL,0                                                          
         BE    EXIT                                                             
         IC    RF,ADRLN                                                         
         CLI   ADREL,ADRELQ                                                     
         BE    *+8                                                              
         BXH   R3,RF,COA02                                                      
         SH    RF,=Y(ADRADD1+1-ADRELD)                                          
         BM    EXIT                                                             
         EX    RF,*+4                                                           
         MVC   BPDATA(0),ADRADD1                                                
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
CC       DS    0H                  * CLIENT CODE *                              
         MVC   BPDATA(L'BCCLICOD),BCCLICOD                                      
         B     EXIT                                                             
*                                                                               
CN       DS    0H                  * CLIENT NAME *                              
         MVC   BPDATA(L'BCCLINAM),BCCLINAM                                      
         B     EXIT                                                             
*                                                                               
PC       DS    0H                  * PRODUCT CODE *                             
         XR    RF,RF                                                            
         IC    RF,BCCLILEN                                                      
         LA    RF,BCPROCOD(RF)                                                  
         IC    RE,BCPROLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   BPDATA(0),0(RF)                                                  
         B     EXIT                                                             
*                                                                               
PN       DS    0H                  * PRODUCT NAME *                             
         MVC   BPDATA(L'BCPRONAM),BCPRONAM                                      
         B     EXIT                                                             
*                                                                               
JC       DS    0H                  * JOB CODE *                                 
         XR    RF,RF                                                            
         IC    RF,BCPROLEN                                                      
         LA    RF,BCJOBCOD(RF)                                                  
         IC    RE,BCJOBLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   BPDATA(0),0(RF)                                                  
         B     EXIT                                                             
*                                                                               
JN       DS    0H                  * JOB NAME *                                 
         MVC   BPDATA(L'BCJOBNAM),BCJOBNAM                                      
         B     EXIT                                                             
*                                                                               
OP       DS    0H                  * JOB OPEN DATE *                            
         L     R3,GOAJOB                                                        
         LA    R3,ACCORFST(R3)                                                  
         USING JOBELD,R3                                                        
         XR    RF,RF                                                            
OP02     CLI   JOBEL,0                                                          
         BE    EXIT                                                             
         CLI   JOBEL,JOBELQ                                                     
         BE    *+12                                                             
         IC    RF,JOBLN                                                         
         BXH   R3,RF,OP02                                                       
         CLI   JOBLN,JOBLN1Q                                                    
         BNH   EXIT                                                             
         GOTO1 VDATCON,BOPARM,(X'41',JOBODATE),(17,BPDATA)                      
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
CL       DS    0H                  * JOB CLOSE DATE *                           
         L     R3,GOAJOB                                                        
         LA    R3,ACCORFST(R3)                                                  
         USING JOBELD,R3                                                        
         XR    RF,RF                                                            
CL02     CLI   JOBEL,0                                                          
         BE    EXIT                                                             
         CLI   JOBEL,JOBELQ                                                     
         BE    *+12                                                             
         IC    RF,JOBLN                                                         
         BXH   R3,RF,CL02                                                       
         GOTO1 VDATCON,BOPARM,(X'41',JOBCDATE),(17,BPDATA)                      
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
BA       DS    0H                  * BILLING ADDRESS *                          
         GOTO1 VHELLO,BOPARM,(C'G',ACCOUNT),('ADRELQ',GOAJOB),0                 
         CLI   12(R1),0                                                         
         BE    BA02                                                             
         GOTO1 (RF),(R1),,('ADRELQ',GOAPRO),0                                   
         CLI   12(R1),0                                                         
         BE    BA02                                                             
         GOTO1 (RF),(R1),,('ADRELQ',GOACLI),0                                   
         CLI   12(R1),0                                                         
         BNE   EXIT                                                             
BA02     L     R3,12(R1)                                                        
         USING ADRELD,R3                                                        
         XR    RF,RF                                                            
         IC    RF,ADRLN                                                         
         SH    RF,=Y(ADRADD1+1-ADRELD)                                          
         BM    EXIT                                                             
         EX    RF,*+4                                                           
         MVC   BPDATA(0),ADRADD1                                                
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
MC       DS    0H                  * MEDIA CODE *                               
         XR    RF,RF                                                            
         IC    RF,BCPROLEN                                                      
         LA    RF,BCJOBCOD(RF)                                                  
         MVC   BPDATA(1),0(RF)                                                  
         B     EXIT                                                             
*                                                                               
MN       DS    0H                  * MEDIA NAME *                               
         LA    R2,IOKEY                                                         
         USING PMDRECD,R2                                                       
         MVC   PMDKEY,BCSPACES                                                  
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,CUABIN                                                   
         XR    RF,RF                                                            
         IC    RF,BCPROLEN                                                      
         LA    RF,BCJOBCOD(RF)                                                  
         MVC   PMDKMED,0(RF)                                                    
         GOTO1 AIO,IOREAD+IOACCMST+IO3                                          
         BNE   EXIT                                                             
         L     R2,AIO3                                                          
         LA    R3,PMDRFST                                                       
         XR    RF,RF                                                            
MN02     CLI   0(R3),0                                                          
         BE    EXIT                                                             
         USING PMDELD,R3                                                        
         CLI   PMDEL,PMDELQ                                                     
         BNE   MN04                                                             
         MVC   BPDATA(L'PMDDESC),PMDDESC                                        
         CLI   BEWLANG,0                                                        
         BE    EXIT                                                             
         B     MN08                                                             
         DROP  R3                                                               
         USING XNMELD,R3                                                        
MN04     CLI   XNMEL,XNMELQ        TEXT EXTRA NAME EL FOR DIFF LANGUAGE         
         BNE   MN08                                                             
         CLC   XNMSTAT,BEWLANG                                                  
         BE    MN06                                                             
         CLI   BEWLANG,1           IS ENGLISH REQUIRED                          
         BNE   MN08                                                             
         CLI   XNMSTAT,0                                                        
         BNE   MN08                                                             
MN06     MVC   BPDATA(L'PMDDESC),BCSPACES                                       
         IC    RF,XNMSUBL                                                       
         AHI   RF,-2                                                            
         BM    EXIT                                                             
         EX    RF,*+4                                                           
         MVC   BPDATA(0),XNMSUBN                                                
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
MN08     IC    RF,1(R3)                                                         
         BXH   R3,RF,MN02                                                       
         DROP  R2                                                               
*                                                                               
BN       DS    0H                  * BILL NUMBER *                              
         OC    BLHBILD,BLHBILD     TEST LIVE BILL                               
         BZ    *+14                                                             
         MVC   BPDATA(L'BLHBLNO),BLHBLNO                                        
         B     EXIT                                                             
         LHI   RF,UC6DRAFT-TWAD                                                 
         LA    RF,TWAD(RF)                                                      
         MVC   BPDATA(L'UC6DRAFT),0(RF)                                         
         B     EXIT                                                             
*                                                                               
BD       DS    0H                  * BILL DATE *                                
         GOTO1 VDATCON,BOPARM,(X'42',BLHTRND),(17,BPDATA)                       
         B     EXIT                                                             
*                                                                               
DD       DS    0H                  * DUE DATE *                                 
         GOTO1 VDATCON,BOPARM,(X'42',BLHDUED),(17,BPDATA)                       
         B     EXIT                                                             
*                                                                               
CUR      DS    0H                  * BILL CURRENCY *                            
         MVC   BPDATA(L'BLHCUR),BLHCUR                                          
         B     EXIT                                                             
*                                                                               
CV       DS    0H                  * CLIENT VAT NUMBER *                        
         GOTO1 VHELLO,BOPARM,(C'G',ACCOUNT),('FFTELQ',GOAPRO),         *        
               (L'FFTTYPE,=AL1(FFTTAXNO))                                       
         CLI   12(R1),0                                                         
         BE    CV02                                                             
         GOTO1 (RF),(R1),,('FFTELQ',GOACLI),(L'FFTTYPE,=AL1(FFTTAXNO))          
         CLI   12(R1),0                                                         
         BNE   EXIT                                                             
CV02     L     R3,12(R1)                                                        
         USING FFTELD,R3                                                        
         IC    RE,FFTDLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   BPDATA(0),FFTDATA                                                
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
OA       DS    0H                  * OFFICE (IDI DESTINATION) ADDRESS *         
         MVC   BPDATA(L'BCDSTAD1*3),BCDSTAD1                                    
         B     EXIT                                                             
*                                                                               
JCO      DS    0H                  * JOB COMMENTS *                             
         L     R3,GOAJOB                                                        
         LA    R3,ACCORFST(R3)                                                  
         USING SCMELD,R3                                                        
         XR    RF,RF                                                            
JCO02    CLI   SCMEL,0                                                          
         BE    EXIT                                                             
         IC    RF,SCMLN                                                         
         CLI   SCMEL,SCMELQ                                                     
         BE    *+8                                                              
         BXH   R3,RF,JCO02                                                      
         AHI   RF,-(SCMLN1Q+1)                                                  
         BM    EXIT                                                             
         EX    RF,*+4                                                           
         MVC   BPDATA(0),SCMNARR                                                
         LA    RF,1(RF)                                                         
         STH   RF,BPDATALN                                                      
         B     EXIT                                                             
*                                                                               
DC       DS    0H                  * DEBTORS CODE *                             
         MVC   BPDATA(L'PPRRECVA),BCCMPPRF+(PPRRECVA-PPRELD)                    
         L     R3,BPABHDR                                                       
         USING SPAELD,R3                                                        
         XR    RF,RF                                                            
DC02     CLI   SPAEL,0                                                          
         BE    EXIT                                                             
         CLI   SPAEL,SPAELQ                                                     
         BE    *+12                                                             
         IC    RF,SPALN                                                         
         BXH   R3,RF,DC02                                                       
         MVC   BPDATA(L'SPAAACT),SPAAACT                                        
         DROP  R3                                                               
         B     EXIT                                                             
*                                                                               
BST      DS    0H                  * BILL STATUS *                              
         OC    BLHBILD,BLHBILD     TEST LIVE BILL                               
         BNZ   EXIT                                                             
         LHI   RF,UC6DRAFT-TWAD                                                 
         LA    RF,TWAD(RF)                                                      
         MVC   BPDATA(L'UC6DRAFT),0(RF)                                         
         B     EXIT                                                             
*                                                                               
AG1T     DS    0H                  * ACCOUNT GROUP 1 TYPE *                     
         MVI   BOBYTE1,1                                                        
         B     *+8                                                              
AG2T     MVI   BOBYTE1,2           * ACCOUNT GROUP 2 TYPE *                     
         L     R3,GOACOMP                                                       
         LA    R3,ACCORFST(R3)                                                  
         USING FFTELD,R3                                                        
         XR    RF,RF                                                            
AG02     CLI   FFTEL,0                                                          
         BE    EXIT                                                             
         IC    RF,FFTLN                                                         
         CLI   FFTEL,FFTELQ                                                     
         BNE   AG08                                                             
         CLI   FFTTYPE,FFTTAAGR                                                 
         BNE   AG08                                                             
         CLC   FFTSEQ,BOBYTE1                                                   
         BE    AG10                                                             
AG08     BXH   R3,RF,AG02                                                       
*                                                                               
AG10     ICM   RF,1,FFTDLEN                                                     
         BZ    EXIT                                                             
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   BPDATA(0),FFTDATA                                                
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
AG1C     DS    0H                  * ACCOUNT GROUP 1 CODE *                     
         MVI   BOBYTE1,AGRLN1Q                                                  
         B     *+8                                                              
AG2C     MVI   BOBYTE1,AGRLN2Q     * ACCOUNT GROUP 2 CODE *                     
         L     R3,GOAPRO                                                        
         LA    R3,ACCORFST(R3)                                                  
         USING AGRELD,R3                                                        
         XR    RF,RF                                                            
AGC02    CLI   AGREL,0                                                          
         BE    EXIT                                                             
         CLI   AGREL,AGRELQ                                                     
         BE    *+12                                                             
         IC    RF,AGRLN                                                         
         BXH   R3,RF,AGC02                                                      
         CLC   AGRLN,BOBYTE1       TEST DATA CARRIED IN THIS ELEMENT            
         BL    EXIT                                                             
         IC    RF,BOBYTE1                                                       
         AHI   RF,-L'AGRAGR1                                                    
         AR    R3,RF                                                            
         MVC   BPDATA(L'AGRAGR1),0(R3)                                          
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
*                                                                               
AG1N     DS    0H                  * ACCOUNT GROUP 1 NAME *                     
         MVI   BOBYTE1,AGRLN1Q                                                  
         B     *+8                                                              
AG2N     MVI   BOBYTE1,AGRLN2Q     * ACCOUNT GROUP 2 NAME *                     
         L     R3,GOAPRO                                                        
         LA    R3,ACCORFST(R3)                                                  
         USING AGRELD,R3                                                        
         XR    RF,RF                                                            
AGN02    CLI   AGREL,0                                                          
         BE    EXIT                                                             
         CLI   AGREL,AGRELQ                                                     
         BE    *+12                                                             
         IC    RF,AGRLN                                                         
         BXH   R3,RF,AGN02                                                      
         CLC   AGRLN,BOBYTE1       TEST DATA CARRIED IN THIS ELEMENT            
         BL    EXIT                                                             
         IC    RF,BOBYTE1                                                       
         AHI   RF,-L'AGRAGR1                                                    
         DROP  R3                                                               
         AR    R3,RF                                                            
*                                                                               
         LA    R2,IOKEY                                                         
         USING AGRRECD,R2                                                       
         XC    AGRKEY,AGRKEY                                                    
         MVI   AGRKTYP,AGRKTYPQ                                                 
         MVC   AGRKCPY,CUABIN                                                   
         MVI   AGRKGTYP,AGRKGTY1                                                
         CLI   BOBYTE1,AGRLN1Q                                                  
         BE    *+8                                                              
         MVI   AGRKGTYP,AGRKGTY2                                                
         MVC   AGRKAGR,0(R3)                                                    
         GOTO1 AIO,IOREAD+IOACCMST+IO3                                          
         BNE   EXIT                                                             
         L     R2,AIO3                                                          
         LA    R3,AGRRFST                                                       
         USING NAMELD,R3                                                        
         XR    RF,RF                                                            
AGN04    CLI   NAMEL,0                                                          
         BE    EXIT                                                             
         IC    RF,NAMLN                                                         
         CLI   NAMEL,NAMELQ                                                     
         BE    *+8                                                              
         BXH   R3,RF,AGN04                                                      
         AHI   RF,-(NAMLN1Q+1)                                                  
         BM    EXIT                                                             
         EX    RF,*+4                                                           
         MVC   BPDATA(0),NAMEREC                                                
         B     EXIT                                                             
         DROP  R2,R3                                                            
*                                                                               
CPJ      DS    0H                  * JOB CODE (CLIPROJOB *                      
         MVC   BPDATA(L'BCJOBCOD),BCJOBCOD                                      
         MVC   BPDATALN,=AL2(L'BCJOBCOD)                                        
         B     EXIT                                                             
*                                                                               
CPJSEP   DS    0H                  * JOB CODE (CLI/PRO/JOB) *                   
         XR    R2,R2                                                            
         IC    R2,BCCLILEN         R2 = L(CLIENT CODE)                          
         XR    R3,R3                                                            
         IC    R3,BCPROLEN                                                      
         XR    R4,R4                                                            
         IC    R4,BCJOBLEN                                                      
         SR    R4,R3               R4 = L(JOB CODE)                             
         SR    R3,R2               R3 = L(PRODUCT CODE)                         
*                                                                               
         LA    RF,BPDATA           RF = A(OUTPUT)                               
         LA    R1,BCJOBCOD         R1 = A(CLI/PRO/JOB)                          
*                                                                               
         EX    R2,*+4              COPY CLIENT CODE + /                         
         MVC   0(0,RF),0(R1)                                                    
         AR    RF,R2                                                            
         AR    R1,R2                                                            
         MVI   0(RF),C'/'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
         EX    R3,*+4              COPY PRODUCT CODE + /                        
         MVC   0(0,RF),0(R1)                                                    
         AR    RF,R3                                                            
         AR    R1,R3                                                            
         MVI   0(RF),C'/'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
         BCTR  R4,0                                                             
         EX    R4,*+4                                                           
         MVC   0(0,RF),0(R1)                                                    
         B     EXIT                                                             
*                                                                               
UF1      LA    R0,1                * USER FIELD 1-8 *                           
         B     UF00                                                             
UF2      LA    R0,2                                                             
         B     UF00                                                             
UF3      LA    R0,3                                                             
         B     UF00                                                             
UF4      LA    R0,4                                                             
         B     UF00                                                             
UF5      LA    R0,5                                                             
         B     UF00                                                             
UF6      LA    R0,6                                                             
         B     UF00                                                             
UF7      LA    R0,7                                                             
         B     UF00                                                             
UF8      LA    R0,8                                                             
*                                                                               
UF00     L     R3,GOAJOB                                                        
         LA    R3,ACCORFST(R3)                                                  
         USING UFSELD,R3                                                        
         XR    RF,RF                                                            
UF02     CLI   UFSEL,0                                                          
         BE    EXIT                                                             
         IC    RF,UFSLN                                                         
         CLI   UFSEL,UFSELQ                                                     
         BNE   UF08                                                             
         BCT   R0,UF08                                                          
         B     UF10                                                             
UF08     BXH   R3,RF,UF02                                                       
*                                                                               
UF10     MVC   BPDATA(L'UFSDESC),UFSDESC                                        
         MVI   BPDATA+L'UFSDESC,C' '                                            
         AHI   RF,-(UFSDATA-UFSELD+1)                                           
         BM    EXIT                                                             
         EX    RF,*+4                                                           
         MVC   BPDATA+L'UFSDESC+1(0),UFSDATA                                    
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
ACCMST   DC    C'ACCMST '                                                       
ACCOUNT  DC    C'ACCOUNT'                                                       
ADDCODE  DC    C'ADD=CODE'                                                      
ADDEND   DC    C'ADD=END'                                                       
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* FIELD TABLE                                                         *         
***********************************************************************         
         SPACE 1                                                                
FLDTABD  DSECT                                                                  
FLDCODE  DS    CL8                                                              
FLDROUT  DS    AL2                                                              
FLDINDS1 DS    XL1                                                              
FLDIUPDT EQU   X'80'               FIELD IS UPDATABLE                           
FLDTABL  EQU   *-FLDTABD                                                        
         SPACE 1                                                                
CLB67    CSECT                                                                  
FLDTAB   DS    0XL(FLDTABL)                                                     
*                                                                               
         DC    CL8'AN'             ORIGIN NAME                                  
         DC    AL2(AN-CLB67)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'AA'             ORIGIN ADDRESS                               
         DC    AL2(AA-CLB67)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'TD'             TODAY'S DATE                                 
         DC    AL2(TD-CLB67)                                                    
         DC    AL1(FLDIUPDT)                                                    
*                                                                               
         DC    CL8'PG'             PAGE NUMBER                                  
         DC    AL2(PG-CLB67)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'CC'             CLIENT CODE                                  
         DC    AL2(CC-CLB67)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'CN'             CLIENT NAME                                  
         DC    AL2(CN-CLB67)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'PC'             PRODUCT CODE                                 
         DC    AL2(PC-CLB67)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'PN'             PRODUCT NAME                                 
         DC    AL2(PN-CLB67)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'JC'             JOB CODE                                     
         DC    AL2(JC-CLB67)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'JN'             JOB NAME                                     
         DC    AL2(JN-CLB67)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'OP'             JOB OPEN DATE                                
         DC    AL2(OP-CLB67)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'CL'             JOB CLOSE DATE                               
         DC    AL2(CL-CLB67)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'BA'             BILLING ADDRESS                              
         DC    AL2(BA-CLB67)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'MC'             MEDIA CODE                                   
         DC    AL2(MC-CLB67)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'MN'             MEDIA NAME                                   
         DC    AL2(MN-CLB67)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'CON'            COMPANY NAME                                 
         DC    AL2(CON-CLB67)                                                   
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'COA'            COMPANY ADDRESS                              
         DC    AL2(COA-CLB67)                                                   
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'BN'             BILL NUMBER                                  
         DC    AL2(BN-CLB67)                                                    
         DC    AL1(FLDIUPDT)                                                    
*                                                                               
         DC    CL8'BD'             BILL DATE                                    
         DC    AL2(BD-CLB67)                                                    
         DC    AL1(FLDIUPDT)                                                    
*                                                                               
         DC    CL8'DD'             DUE DATE                                     
         DC    AL2(DD-CLB67)                                                    
         DC    AL1(FLDIUPDT)                                                    
*                                                                               
         DC    CL8'CUR'            BILL CURRENCY                                
         DC    AL2(CUR-CLB67)                                                   
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'CV'             CLIENT VAT NUMBER                            
         DC    AL2(CV-CLB67)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'JCO'            JOB COMMENTS                                 
         DC    AL2(JCO-CLB67)                                                   
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'OA'             OFFICE (IDI DESTINATION) ADDRESS             
         DC    AL2(OA-CLB67)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'DC'             DEBTORS CODE                                 
         DC    AL2(DC-CLB67)                                                    
         DC    AL1(FLDIUPDT)                                                    
*                                                                               
         DC    CL8'BST'            BILL STATUS                                  
         DC    AL2(BST-CLB67)                                                   
         DC    AL1(FLDIUPDT)                                                    
*                                                                               
         DC    CL8'AG1T'           ACCOUNT GROUP 1 TYPE                         
         DC    AL2(AG1T-CLB67)                                                  
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'AG1C'           ACCOUNT GROUP 1 CODE                         
         DC    AL2(AG1C-CLB67)                                                  
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'AG1N'           ACCOUNT GROUP 1 NAME                         
         DC    AL2(AG1N-CLB67)                                                  
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'AG2T'           ACCOUNT GROUP 2 TYPE                         
         DC    AL2(AG2T-CLB67)                                                  
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'AG2C'           ACCOUNT GROUP 2 CODE                         
         DC    AL2(AG2C-CLB67)                                                  
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'AG2N'           ACCOUNT GROUP 2 NAME                         
         DC    AL2(AG2N-CLB67)                                                  
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'CPJ'            JOB CODE (CLIPROJOB)                         
         DC    AL2(CPJ-CLB67)                                                   
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'CPJSEP'         JOB CODE (CLI/PRO/JOB)                       
         DC    AL2(CPJSEP-CLB67)                                                
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'UF1   '         USER FIELD 1                                 
         DC    AL2(UF1-CLB67)                                                   
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'UF2   '         USER FIELD 2                                 
         DC    AL2(UF2-CLB67)                                                   
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'UF3   '         USER FIELD 3                                 
         DC    AL2(UF3-CLB67)                                                   
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'UF44  '         USER FIELD 4                                 
         DC    AL2(UF1-CLB67)                                                   
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'UF5   '         USER FIELD 5                                 
         DC    AL2(UF5-CLB67)                                                   
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'UF6   '         USER FIELD 6                                 
         DC    AL2(UF6-CLB67)                                                   
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'UF7   '         USER FIELD 7                                 
         DC    AL2(UF7-CLB67)                                                   
         DC    AL1(0)                                                           
*                                                                               
         DC    CL8'UF8   '         USER FIELD 8                                 
         DC    AL2(UF8-CLB67)                                                   
         DC    AL1(0)                                                           
*                                                                               
FLDTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
* ACCLBLINK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBLINK                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
BPWORKD  DSECT                                                                  
BPPARMS  DS    0XL16                                                            
BPAACT   DS    A                   A(ACTION                                     
BPAINP   DS    A                   A(INPUT RECORD)                              
BPAOUT   DS    A                   A(OUTPUT RECORD)                             
BPABHDR  DS    A                   A(BILL HEADER)                               
*                                                                               
BPAR1    DS    A                   A(CALLERS R1)                                
*                                                                               
BPACT    DS    CL1                                                              
BPACTADD EQU   C'A'                ADD NEW BILL PANELS                          
BPACTUPD EQU   C'U'                UPDATE EXISTING BILL PANELS                  
*                                                                               
BPINDS   DS    XL1                 INDICATOR BYTE                               
BPIPAN   EQU   X'80'               RECORD CONTAINS PANELS                       
BPIUPD   EQU   X'40'               RECORD HAS BEEN CHANGED                      
*                                                                               
BPSAVKEY DS    XL42                SAVE KEY FOR READ SEQUENCE                   
*                                                                               
BPDATA   DS    XL256               DATA                                         
BPDATALN DS    H                                                                
BPELS    DS    XL1024                                                           
BPINP    DS    XL2048              COPY OF INPUT RECORD                         
*                                                                               
BPWORKL  EQU   *-BPWORKD                                                        
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049ACCLB67   08/16/00'                                      
         END                                                                    
