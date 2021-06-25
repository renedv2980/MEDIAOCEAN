*          DATA SET ACCLB65    AT LEVEL 098 AS OF 08/16/00                      
*PHASE T62165A                                                                  
*&&      SET   NOP=N                                                            
CLB65    TITLE '- PC COMMS - BUILD BILL TOTALS'                                 
CLB65    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB65**,R8,CLEAR=YES                                          
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK                                                      
         USING BTWORKD,RC                                                       
         ST    R7,APBLK                                                         
         L     R6,ALINK                                                         
         USING LINKD,R6                                                         
*                                                                               
         BAS   RE,GETPBLK          GET VALUES FROM PBLKD                        
         BE    *+6                                                              
         DC    H'0'                                                             
         BNE   EXIT                                                             
         BAS   RE,GETFMT           GET VALUES FROM FORMAT HEADER                
         BE    *+6                                                              
         DC    H'0'                                                             
         BNE   EXIT                                                             
*                                                                               
* ??     GOTO1 PRCTOT,BEDKLCPQ     COVER PAGE TOTALS                            
         GOTO1 PRCTOT,BEDKLMNQ     MAIN BILL TOTALS                             
* ??     GOTO1 PRCTOT,BEDKLSMQ     SUMMARY PAGE TOTALS                          
*                                                                               
         B     EXITY                                                            
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
* GET VALUES FROM PBLK                                                *         
***********************************************************************         
         SPACE 1                                                                
GETPBLK  NTR1  ,                                                                
         L     R3,APBLK                                                         
         USING PBLKD,R3                                                         
*                                                                               
         MVC   BTBILNUM,PBDRAFT#                                                
         CLI   PBMODE,PBLIVEQ                                                   
         BNE   *+10                                                             
         MVC   BTBILNUM,PBLIVE#                                                 
         ZAP   BTNET,PBBHAPN                                                    
         ZAP   BTCOM,PBBHAPC                                                    
         ZAP   BTSRC,PBTOTAS                                                    
         ZAP   BTSRCPC,PBSRCPC                                                  
         ZAP   BTDSC,PBTOTAD                                                    
         ZAP   BTDSCPC,PBDSCPC                                                  
         ZAP   BTVAT,PBTOTAV                                                    
         ZAP   BTVATAGY,PBTOTAV                                                 
         CLC   CSBILCUR,CSCPYCUR   TEST FOREIGN CURRENCY BILLING                
         BE    GETPBLKX                                                         
         ZAP   BTSRC,PBTOTFS       YES - GET FOREIGN CURRENCY VALUES            
         ZAP   BTDSC,PBTOTFD                                                    
         ZAP   BTVAT,PBTOTFV                                                    
*                                                                               
GETPBLKX B     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GET FORMAT HEADER AND EXTRACT VALUES                                *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
GETFMT   NTR1  ,                                                                
K        USING BFMRECD,IOKEY                                                    
         XC    K.BFMKEY,K.BFMKEY                                                
         MVI   K.BFMKTYP,BFMKTYPQ                                               
         MVC   K.BFMKCPY,CUABIN                                                 
         MVI   K.BFMKSUB,BFMKSUBQ                                               
         MVC   K.BFMKFMT,BEWBLH+(BLHFORM-BLHELD)                                
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BNE   EXITN                                                            
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BNE   EXITN                                                            
         L     R2,AIO1                                                          
         USING BFMRECD,R2                                                       
         MVC   BTFMTHDR,BFMKEY                                                  
*                                                                               
         LA    R3,BFMRFST                                                       
         USING BOFELD,R3                                                        
         XR    RF,RF                                                            
GFMT02   CLI   BOFEL,0                                                          
         BE    EXITN                                                            
         IC    RF,BOFLN                                                         
         CLI   BOFEL,BOFELQ                                                     
         BE    *+8                                                              
         BXH   R3,RF,GFMT02                                                     
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SVBOFEL(0),BOFELD   SAVE BILL HEADER ELEMENT                     
*                                                                               
GETFMTX  B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS TOTAL                                                       *         
*                                                                     *         
* NTRY: R1 = LEVEL OF TOTAL                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
PRCTOT   NTR1  ,                                                                
K        USING BFMRECD,IOKEY       READ FORMAT TOTAL RECORD                     
         MVC   K.BFMKEY,BTFMTHDR                                                
         STCM  R1,3,K.BFMKLVL                                                   
         MVC   K.BFMKWHER,=AL2(BFMKWBTQ)                                        
         GOTO1 AIO,IOREAD+IOACCDIR+IO3                                          
         BNE   EXITN                                                            
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         BNE   EXITN                                                            
         L     R3,AIO3                                                          
         LA    R3,BFMRFST-BFMRECD(R3)                                           
         USING BTTELD,R3                                                        
         XR    RF,RF                                                            
PTOT02   CLI   BTTEL,0                                                          
         BE    EXITN                                                            
         IC    RF,BTTLN                                                         
         CLI   BTTEL,BTTELQ                                                     
         BE    *+8                                                              
         BXH   R3,RF,PTOT02                                                     
         XC    SVBTTEL,SVBTTEL                                                  
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SVBTTEL(0),BTTELD                                                
         DROP  R3,K                                                             
*                                                                               
         GOTO1 PRCMBT              PROCESS MAIN BILL TOTALS                     
         GOTO1 PRCVAT              PROCESS VAT ANALYSIS                         
*GOTO1 ROUTINE TO DO PREVIOUS BILLS                                             
*                                                                               
PRCTOTX  B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS MAIN BILL TOTALS                                            *         
*                                                                     *         
* NTRY: IO3 = FORMAT BILL TOTALS RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING BTTELD,SVBTTEL                                                   
PRCMBT   NTR1  ,                                                                
K        USING BEDRECD,IOKEY                                                    
         MVC   K.BEDKEY,BEWHDRKY                                                
         L     RF,AIO3                                                          
         MVC   K.BEDKLVL,BFMKLVL-BFMKEY(RF)                                     
         MVC   K.BEDKWHER,=AL2(BEDKWBTQ)                                        
         NI    BTINDS,FF-BTIADD                                                 
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BNE   PMBT02                                                           
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,AIO1                                                          
         LH    RF,BEDRLEN-BEDRECD(RE)                                           
         L     R0,AIO2                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE               COPY IO1 TO IO2                              
         B     PMBT04                                                           
*                                                                               
PMBT02   DS    0H                                                               
         OI    BTINDS,BTIADD                                                    
         L     R2,AIO2                                                          
         USING BEDRECD,R2                                                       
         XC    BEDRECD(BEDRFST-BEDRECD+1),BEDRECD                               
         MVC   BEDKEY,IOKEYSAV                                                  
         MVC   BEDRLEN,=AL2(BEDRFST-BEDRECD+1)                                  
         DROP  R2                                                               
*                                                                               
PMBT04   DS    0H                                                               
         L     R2,AIO1                                                          
         USING BEDRECD,R2                                                       
         XC    BEDRECD(BEDRFST-BEDRECD+1),BEDRECD                               
         MVC   BEDKEY,IOKEYSAV                                                  
         LA    R3,BEDRFST                                                       
         USING BFPELD,R3                                                        
         XC    BFPELD(BFPLNQ),BFPELD                                            
         MVI   BFPEL,BFPELQ                                                     
         MVI   BFPLN,BFPLNQ                                                     
         STCM  R3,15,ABFPEL                                                     
         DROP  R3                                                               
         LA    R3,BFPLNQ(R3)                                                    
         MVI   0(R3),0                                                          
         SR    R3,R2                                                            
         LA    R3,1(R3)                                                         
         STH   R3,BEDRLEN                                                       
         MVI   LINENO,0                                                         
*                                                                               
         LA    R4,MBTTAB                                                        
         USING MBTTABD,R4                                                       
PMBT12   CLI   MBTTABD,EOT                                                      
         BE    PMBT30                                                           
*                                                                               
D        USING BFMELD,BFMELD1                                                   
         GOTO1 CPYBFM,BOPARM,AIO2,MBTPANEL,D.BFMELD                             
         BE    PMBT14              GET DESCRIPTION FROM EXISTING BILL           
         GOTO1 (RF),(R1),AIO3,MBTPANEL,D.BFMELD                                 
         BNE   PMBT28              OR FROM FORMAT                               
*                                                                               
PMBT14   DS    0H                                                               
X        USING BFMELD,BFMELD2                                                   
         MVC   X.BFMELD(BFMLNQ),D.BFMELD                                        
         MVI   X.BFMLN,BFMLNQ                                                   
         XC    X.BFMTLEN,X.BFMTLEN                                              
         MVI   X.BFMTYPE,BFMTDATA                                               
         NI    X.BFMSTAT1,FF-(BFMSNUM+BFMSTOT)                                  
         MVI   X.BFMWTH,0                                                       
         MVI   X.BFMALN,BFMALFTQ                                                
*                                                                               
A        USING BFMELD,BFMELD3                                                   
         MVC   A.BFMELD(BFMLNQ),D.BFMELD                                        
         MVI   A.BFMLN,BFMLNQ                                                   
         XC    A.BFMTLEN,X.BFMTLEN                                              
         MVI   A.BFMTYPE,BFMTDATA                                               
         MVI   A.BFMWTH,0                                                       
         MVI   A.BFMALN,BFMARGTQ                                                
*                                                                               
         BAS   RE,PRCMBTR          CALL ROUTINE                                 
         BNE   PMBT28                                                           
*                                                                               
         IC    RE,LINENO           SET LINE NUMBER                              
         LA    RE,1(RE)                                                         
         STC   RE,LINENO                                                        
         STC   RE,D.BFMTOP                                                      
         STC   RE,X.BFMTOP                                                      
         STC   RE,A.BFMTOP                                                      
*                                                                               
         XR    R0,R0                                                            
         IC    R0,D.BFMLEFT        R0 = LEFT POSITION                           
         MVC   D.BFMLEFT,BTTMAINL                                               
         XR    R3,R3                                                            
         IC    R3,BTTMAINW                                                      
         LA    R3,AMTWTHQ(R3)      R3 = OVERALL WIDTH                           
         XR    RE,RE                                                            
         IC    RE,X.BFMWTH                                                      
         SR    R3,RE                                                            
         IC    RE,A.BFMWTH                                                      
         SR    R3,RE                                                            
         STC   R3,D.BFMWTH                                                      
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),AIO1,D.BFMELD,ADDEND                 
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         AR    R0,R3                                                            
*                                                                               
         XR    RE,RE                                                            
         ICM   R3,1,X.BFMWTH       TEST EXTRA DESC. EXISTS                      
         BZ    PMBT20                                                           
         STC   R0,X.BFMLEFT        YES - ADD TO RECORD                          
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),AIO1,X.BFMELD,ADDEND                 
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         AR    R0,R3                                                            
*                                                                               
PMBT20   CLI   A.BFMWTH,0          TEST AMOUNT EXISTS                           
         BE    PMBT28                                                           
         STC   R0,A.BFMLEFT        YES - ADD TO RECORD                          
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),AIO1,A.BFMELD,ADDEND                 
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PMBT28   LA    R4,MBTTABDL(R4)                                                  
         B     PMBT12                                                           
*                                                                               
PMBT30   DS    0H                                                               
         ICM   R3,15,ABFPEL        UPDATE NUMBER OF LINES IN PARAGRAPH          
         USING BFPELD,R3                                                        
         IC    RE,LINENO                                                        
         STC   RE,BFPHGTMN                                                      
         LA    RE,1(RE)                                                         
         STC   RE,BFPHGT                                                        
         DROP  R3                                                               
*                                                                               
         TM    BTINDS,BTIADD       TEST ADDING RECORD                           
         BZ    PMBT32                                                           
         GOTO1 AIO,IOADD+IOACCMST+IO1                                           
         BE    PRCMBTX                                                          
         DC    H'0'                                                             
*                                                                               
PMBT32   DS    0H                  TEST IF RECORD HAS CHANGED                   
         L     RE,AIO2                                                          
         CLC   BEDRLEN,BEDRLEN-BEDRECD(RE)                                      
         BNE   PMBT34                                                           
         LH    RF,BEDRLEN                                                       
         LA    R0,BEDRECD                                                       
         LR    R1,RF                                                            
         CLCL  RE,R0                                                            
         BE    PRCMBTX                                                          
PMBT34   OI    BEWINDS,BEWIUTOT                                                 
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    PRCMBTX                                                          
         DC    H'0'                                                             
*                                                                               
PRCMBTX  B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS MAIN BILL TOTALS ROUTINE                                    *         
***********************************************************************         
         SPACE 1                                                                
PRCMBTR  NTR1  ,                                                                
         LH    RF,MBTROUT                                                       
         B     CLB65(RF)                                                        
*                                                                               
MBTNET   DS    0H                  * NET *                                      
         TM    BTTINDS1,BTTINET                                                 
         BZ    EXITN                                                            
         GOTO1 FMTAMT,BOPARM,A.BFMELD,BTNET                                     
         B     EXITY                                                            
         SPACE 1                                                                
MBTCOM   DS    0H                  * COMMISSION *                               
         TM    BTTINDS1,BTTICOM                                                 
         BZ    EXITN                                                            
         GOTO1 FMTAMT,BOPARM,A.BFMELD,BTCOM                                     
         B     EXITY                                                            
         SPACE 1                                                                
MBTGRS   DS    0H                  * GROSS *                                    
         TM    BTTINDS1,BTTIGRS                                                 
         BZ    EXITN                                                            
         ZAP   BODUB1,BTNET                                                     
         AP    BODUB1,BTCOM                                                     
         GOTO1 FMTAMT,BOPARM,A.BFMELD,BODUB1                                    
         B     EXITY                                                            
         SPACE 1                                                                
MBTSCH   DS    0H                  * SURCHARGE *                                
         CP    BTSRC,BCPZERO                                                    
         BE    EXITN                                                            
         GOTO1 FMTAMT,BOPARM,A.BFMELD,BTSRC                                     
         TM    BTTINDS1,BTTISRCP   TEST WANT PERCENTAGE                         
         BZ    EXITY                                                            
         GOTO1 FMTPCT,BOPARM,X.BFMELD,BTSRCPC                                   
         B     EXITY                                                            
         SPACE 1                                                                
MBTDSC   DS    0H                  * DISCOUNT *                                 
         CP    BTDSC,BCPZERO                                                    
         BE    EXITN                                                            
         GOTO1 FMTAMT,BOPARM,A.BFMELD,BTDSC                                     
         TM    BTTINDS1,BTTIDSCP   TEST WANT PERCENTAGE                         
         BZ    EXITY                                                            
         GOTO1 FMTPCT,BOPARM,X.BFMELD,BTDSCPC                                   
         B     EXITY                                                            
         SPACE 1                                                                
MBTVAT   DS    0H                  * VAT *                                      
         TM    BCGLOB1,BCGLVAT                                                  
         BZ    EXITN                                                            
         GOTO1 FMTAMT,BOPARM,A.BFMELD,BTVAT                                     
         B     EXITY                                                            
         SPACE 1                                                                
MBTTRAX1 DS    0H                  * "------" *                                 
         B     EXITY                                                            
         SPACE 1                                                                
MBTINVT  DS    0H                  * INVOICE TOTAL *                            
         ZAP   INVTOT,BTNET                                                     
         AP    INVTOT,BTCOM                                                     
         AP    INVTOT,BTSRC                                                     
         AP    INVTOT,BTDSC                                                     
         AP    INVTOT,BTVAT                                                     
         XR    RF,RF                                                            
         TM    BTTINDS2,BTTICURT                                                
         BZ    *+8                                                              
         LA    RF,FAPICODE                                                      
         GOTO1 FMTAMT,BOPARM,((RF),A.BFMELD),INVTOT                             
         B     EXITY                                                            
         SPACE 1                                                                
MBTPPAY  DS    0H                  * "PLEASE PAY THIS AMOUNT" *                 
         TM    BTTINDS2,BTTIPPAY                                                
         BZ    EXITN                                                            
         CP    INVTOT,BCPZERO                                                   
         BL    EXITN                                                            
         B     EXITY                                                            
         SPACE 1                                                                
MBTCRAM  DS    0H                  * "CREDIT AMOUNT" *                          
         TM    BTTINDS2,BTTICRAM                                                
         BZ    EXITN                                                            
         CP    INVTOT,BCPZERO                                                   
         BNL   EXITN                                                            
         B     EXITY                                                            
         SPACE 1                                                                
MBTTRAX2 DS    0H                  * "------" *                                 
         B     EXITY                                                            
         SPACE 1                                                                
MBTVATA  DS    0H                  * AGENCY VAT AMOUNT *                        
         TM    BCGLOB1,BCGLVAT                                                  
         BZ    EXITN                                                            
         TM    BTTINDS2,BTTIVATA                                                
         BZ    EXITN                                                            
         CLC   CSBILCUR,CSCPYCUR   ONLY DO IF FOREIGN CURRENCY BILLING          
         BE    EXITN                                                            
         GOTO1 FMTAMT,BOPARM,('FAPICODE+FAPIAGY',A.BFMELD),BTVATAGY             
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS MAIN BILL TOTALS                                            *         
*                                                                     *         
* NTRY: IO3 = FORMAT BILL TOTALS RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING BTTELD,SVBTTEL                                                   
PRCVAT   NTR1  ,                                                                
         TM    BTTINDS1,BTTIAVAT   TEST REQUIRED                                
         BZ    EXIT                (MAY HAVE TO DELETE IT)  ??                  
*                                                                               
K        USING BEDRECD,IOKEY                                                    
         MVC   K.BEDKEY,BEWHDRKY                                                
         L     RF,AIO3                                                          
         MVC   K.BEDKLVL,BFMKLVL-BFMKEY(RF)                                     
         MVC   K.BEDKWHER,=AL2(BEDKWVAQ)                                        
         NI    BTINDS,FF-BTIADD                                                 
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    *+12                                                             
         OI    BTINDS,BTIADD       COULD BE DELETED AND NEED RESTORING          
         B     PVAT10                                                           
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,AIO1                                                          
         LH    RF,BEDRLEN-BEDRECD(RE)                                           
         L     R0,AIO2                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE               COPY IO1 TO IO2                              
*                                                                               
PVAT10   DS    0H                  INITIALIZE RECORD IN IO1                     
         L     R2,AIO1                                                          
         USING BEDRECD,R2                                                       
         XC    BEDRECD(BEDRFST-BEDRECD+1),BEDRECD                               
         MVC   BEDKEY,IOKEYSAV                                                  
         LA    R3,BEDRFST                                                       
         USING BFPELD,R3                                                        
         XC    BFPELD(BFPLNQ),BFPELD                                            
         MVI   BFPEL,BFPELQ                                                     
         MVI   BFPLN,BFPLNQ                                                     
         STCM  R3,15,ABFPEL                                                     
         DROP  R3                                                               
         LA    R3,BFPLNQ(R3)                                                    
         MVI   0(R3),0                                                          
         SR    R3,R2                                                            
         LA    R3,1(R3)                                                         
         STH   R3,BEDRLEN                                                       
         MVI   LINENO,0                                                         
*                                                                               
         MVI   LINENO,1            ADD VAT ANALYSIS HEADLINE                    
         MVC   COLNO,BTTVATL                                                    
         GOTO1 PRCAPP,BOPARM,(C'H',VATTABH),BEDRECD                             
*                                                                               
         MVI   LINENO,3            ADD FIELD HEADLINES                          
         MVC   COLNO,BTTVATL                                                    
         LA    R4,VATTAB                                                        
         USING APPTABD,R4                                                       
PVAT12   CLI   APPTABD,EOT                                                      
         BE    PVAT20                                                           
         GOTO1 PRCAPP,BOPARM,(C'H',APPTABD),BEDRECD                             
         LA    R4,APPTABL(R4)                                                   
         B     PVAT12                                                           
*                                                                               
PVAT20   DS    0H                                                               
         MVI   LINENO,4            ADD VAT LINES                                
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         XC    TLKEY,TLKEY         GET VAT RECORDS                              
         MVI   TLKSES,TLKUVATQ                                                  
         LA    R1,TSARDH                                                        
         B     *+8                                                              
PVAT22   LA    R1,TSANXT                                                        
         GOTO1 ATSARIO,(R1)                                                     
         BL    PVAT30                                                           
         CLI   TLKSES,TLKUVATQ                                                  
         BNE   PVAT30                                                           
         IC    RE,LINENO                                                        
         LA    RE,1(RE)                                                         
         STC   RE,LINENO                                                        
         MVC   COLNO,BTTVATL                                                    
         LA    R4,VATTAB                                                        
PVAT24   CLI   APPTABD,EOT                                                      
         BE    PVAT28                                                           
         GOTO1 PRCAPP,BOPARM,APPTABD,BEDRECD                                    
         LA    R4,APPTABL(R4)                                                   
         B     PVAT24                                                           
*                                                                               
PVAT28   DS    0H                                                               
         B     PVAT22                                                           
*                                                                               
PVAT30   DS    0H                                                               
         ICM   R3,15,ABFPEL        UPDATE NUMBER OF LINES IN PARAGRAPH          
         USING BFPELD,R3                                                        
         IC    RE,LINENO                                                        
         STC   RE,BFPHGTMN                                                      
         LA    RE,1(RE)                                                         
         STC   RE,BFPHGT                                                        
         DROP  R3                                                               
*                                                                               
         TM    BTINDS,BTIADD       TEST ADDING RECORD                           
         BZ    PVAT32                                                           
         GOTO1 AIO,IOADD+IOACCMST+IO1                                           
         BE    PRCVATX                                                          
         DC    H'0'                                                             
*                                                                               
PVAT32   DS    0H                                                               
         L     RE,AIO2                                                          
         CLC   BEDRLEN,BEDRLEN-BEDRECD(RE)                                      
         BNE   PVAT34                                                           
         LH    RF,BEDRLEN                                                       
         LA    R0,BEDRECD                                                       
         LR    R1,RF                                                            
         CLCL  RE,R0                                                            
         BE    PRCVATX                                                          
PVAT34   OI    BEWINDS,BEWIUTOT                                                 
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    PRCVATX                                                          
         DC    H'0'                                                             
*                                                                               
PRCVATX  B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VAT ANALYSIS ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING BFMELD,BFMELD1                                                   
VATTYPET DS    0H                  * TEST WANT TYPE *                           
         CLI   CUCTRY,CTRYHOL      THE DUTCH DON'T WANT IT                      
         BNE   EXITY                                                            
         B     EXITN                                                            
         SPACE 1                                                                
VATTYPE  DS    0H                  * TYPE *                                     
         MVC   BFMTEXT(L'TLDUVTNM),TLDUVTNM                                     
         B     EXITY                                                            
         SPACE 1                                                                
VATNETP  DS    0H                  * NET PRE VAT *                              
         ZAP   BOPL81,TLDUVATA     BOPL81 = AGENCY AMOUNT                       
         ZAP   BOPL82,TLDUVTFA     BOPL82 = FOREIGN CURRENCY AMOUNT             
         B     VAMT02                                                           
         SPACE 1                                                                
VATRATE  DS    0H                  * VAT RATE *                                 
         GOTO1 FMTRAT,BOPARM,BFMELD,TLDUVRAT                                    
         B     EXITY                                                            
         SPACE 1                                                                
VATAMT   DS    0H                  * VAT AMOUNT *                               
         ZAP   BOPL81,TLDUVAT      BOPL81 = AGENCY AMOUNT                       
         ZAP   BOPL82,TLDUVTF      BOPL82 = A(FOREIGN CURRENCY AMOUNT)          
*                                                                               
VAMT02   LA    R2,BOPL81                                                        
         XR    RF,RF                                                            
         CLC   CSBILCUR,CSCPYCUR   TEST FOREIGN BILLING                         
         BE    VAMT04                                                           
         LA    RF,FAPICODE                                                      
         TM    BTTINDS1,BTTIAVAG   TEST ALWAYS IN AGENCY                        
         BZ    *+12                                                             
         LA    RF,FAPIAGY(RF)                                                   
         B     VAMT04                                                           
         LA    R2,BOPL82                                                        
VAMT04   GOTO1 FMTAMT,BOPARM,((RF),BFMELD),(R2)                                 
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT AMOUNT ONTO BFMELD                                *         
*                                                                     *         
* NTRY: P1 BYTE 0 = INDICATOR BYTE                                    *         
*                   X'80' ON TO USE AGENCY CURRENCY, NOT BILLING      *         
*                   X'40' ON TO PREFIX WITH CURRENCY CODE             *         
*             1-3 = A(BFMELD)                                         *         
*       P2        = A(PL8 AMOUNT)                                     *         
* EXIT:   BFMTEXT = OUTOUT                                            *         
*         BFMTLEN = L(OUTPUT)                                         *         
*          BFMWTH = WIDTH OF FIELD                                    *         
*          BFMTLN = NEW L(ELEMENT)                                    *         
***********************************************************************         
         SPACE 1                                                                
FMTAMT   NTR1  ,                                                                
         MVC   FAPARMS,0(R1)                                                    
*                                                                               
         XR    R3,R3                                                            
         ICM   R3,7,FAPABFM                                                     
         USING BFMELD,R3                                                        
         MVC   FACUR,CSCURBIL                                                   
         TM    FAPINDS,FAPIAGY                                                  
         BZ    *+10                                                             
         MVC   FACUR,CSCURCPY                                                   
         ICM   RF,15,FAPAAMT                                                    
         ZAP   FAPL8,0(8,RF)       COPY AMOUNT                                  
*                                                                               
         MVC   BFMTEXT(50),BCSPACES                                             
*                                                                               
         TM    FAPINDS,FAPICODE    TEST REQUIRE CURRENCY CODE                   
         BO    FAMT02                                                           
*&&UK                                                                           
         CURED (P8,FAPL8),(30,BFMTEXT),FACUR,COMMAS=YES,               *        
               MINUS=YES,ALIGN=LEFT                                             
*&&                                                                             
*&&US                                                                           
         EDIT  (P8,FAPL8),(30,BFMTEXT),2,COMMAS=YES,MINUS=YES,         *        
               ALIGN=LEFT                                                       
*&&                                                                             
         B     FAMT10                                                           
*                                                                               
FAMT02   DS    0H                  CURRENCY CODE REQUIRED                       
*&&UK                                                                           
         CLI   CUCTRY,CTRYGER                                                   
         BE    FAMT04                                                           
         CURED (P8,FAPL8),(30,BFMTEXT),FACUR,COMMAS=YES,               *        
               MINUS=YES,ALIGN=LEFT,CURSYMB=YES                                 
         B     FAMT10                                                           
*                                                                               
FAMT04   DS    0H                                                               
         CURED (P8,FAPL8),(30,BFMTEXT+L'CURTCUR+2),FACUR,COMMAS=YES,   *        
               MINUS=YES,ALIGN=LEFT                                             
         MVC   BFMTEXT(L'CURTCUR),FACUR                                         
         AHI   R0,L'CSBILCUR+2                                                  
*&&                                                                             
*&&US                                                                           
         EDIT  (P8,FAPL8),(30,BFMTEXT),2,COMMAS=YES,MINUS=YES,         *        
               ALIGN=LEFT,FLOAT=$                                               
*&&                                                                             
FAMT10   DS    0H                                                               
         CLC   FACUR(L'CURTCUR),=C'TRL'  TEST TURKISH LIRA                      
         BNE   FAMT12                                                           
         TM    FAPINDS,FAPICODE    TEST CURRENCY SYMBOL REQUIRED                
         BZ    FAMT12                                                           
         LA    RF,BFMTEXT-1                                                     
         AR    RF,R0                                                            
         MVC   1(1,RF),0(RF)       INSERT "K" FOR 1000S                         
         MVI   0(RF),C'K'          ("999K " / "999K-")                          
         AHI   R0,1                                                             
*                                                                               
FAMT12   DS    0H                  SAVE LENGTH OF TEXT                          
         STH   R0,BFMTLEN                                                       
         MVI   BFMWTH,AMTWTHQ      SET WIDTH TO DEFAULT                         
         CLM   R0,1,BFMWTH                                                      
         BNH   *+8                                                              
         STC   R0,BFMWTH            OR MORE IF DOES NOT FIT                     
         AHI   R0,BFMLNQ                                                        
         STC   R0,BFMLN                                                         
*                                                                               
FMTAMTX  B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT RATE ONTO BFMELD                                  *         
*                                                                     *         
* NTRY: P1        = A(BFMELD)                                         *         
*       P2        = A(XL2 AMOUNT)                                     *         
* EXIT:   BFMTEXT = OUTOUT                                            *         
*         BFMTLEN = L(OUTPUT)                                         *         
*          BFMTLN = NEW L(ELEMENT)                                    *         
***********************************************************************         
         SPACE 1                                                                
FMTRAT   NTR1  ,                                                                
         LM    R3,R4,0(R1)                                                      
         USING BFMELD,R3                                                        
*                                                                               
         MVC   BFMTEXT(12),BCSPACES                                             
         CURED (B2,(R4)),(8,BFMTEXT),2,ALIGN=LEFT                               
         STH   R0,BFMTLEN          SET TEXT LENGTH                              
         AHI   R0,BFMLNQ                                                        
         STC   R0,BFMLN            SET ELEMENT LENGTH                           
*                                                                               
         B     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT PERCENT AMOUNT ONTO BFMELD                        *         
*                                                                     *         
* NTRY:      P1 = A(BFMELD)                                           *         
*            P2 = A(PL8 PERCENTAGE)                                   *         
* EXIT: BFMTEXT = OUTOUT                                              *         
*       BFMTLEN = L(OUTPUT)                                           *         
*        BFMWTH = WIDTH OF FIELD                                      *         
*        BFMTLN = NEW L(ELEMENT)                                      *         
***********************************************************************         
         SPACE 1                                                                
FMTPCT   NTR1  ,                                                                
         LM    R3,R4,0(R1)                                                      
         USING BFMELD,R3                                                        
*                                                                               
         MVC   BFMTEXT(12),BCSPACES                                             
         LA    R2,BFMTEXT                                                       
         CLI   CUCTRY,CTRYGER      GERMANY + SCANDINAVIA DON'T WANT '@'         
         BE    FPCT02                                                           
         CLI   CUCTRY,CTRYSCA                                                   
         BE    FPCT02                                                           
         MVI   0(R2),C'@'                                                       
         LA    R2,1(R2)                                                         
FPCT02   DS    0H                                                               
         CURED (P8,(R4)),(8,(R2)),2,ALIGN=LEFT                                  
         AR    R2,R0                                                            
         MVI   0(R2),C'%'                                                       
         LA    RE,BFMTEXT-1                                                     
         SR    R2,RE                                                            
         STH   R2,BFMTLEN          SET TEXT LENGTH                              
         LA    R2,1(R2)                                                         
         STC   R2,BFMWTH           WIDTH IS 1 MORE FOR EXTRA SPACE              
         LA    R2,BFMLNQ-1(R2)                                                  
         STC   R2,BFMLN            SET ELEMENT LENGTH                           
*                                                                               
         B     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO COPY BFMELD                                              *         
*                                                                     *         
* NTRY: P1 = A(RECORD)                                                *         
*       P2 = A(PANEL CODE)                                            *         
*       P3 = A(AREA TO COPY BFMEL)                                    *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
CPYBFM   NTR1  ,                                                                
         LM    R2,R4,0(R1)                                                      
         USING BFMRECD,R2                                                       
C        USING BFMELD,R4                                                        
*                                                                               
         LA    R1,BFMRFST                                                       
         USING BFMELD,R1                                                        
         XR    RF,RF                                                            
CBFM02   CLI   BFMEL,0                                                          
         BE    EXITN                                                            
         IC    RF,BFMLN                                                         
         CLI   BFMEL,BFMELQ                                                     
         BNE   CBFM08                                                           
         CLI   BFMTYPE,BFMTFREE                                                 
         BNE   CBFM08                                                           
         CLC   BFMPANEL,0(R3)                                                   
         BE    CBFM10                                                           
CBFM08   DS    0H                                                               
         BXH   R1,RF,CBFM02                                                     
*                                                                               
CBFM10   BCTR  RF,0                                                             
         XC    C.BFMELD(255),C.BFMELD                                           
         EX    RF,*+4                                                           
         MVC   C.BFMELD(0),BFMELD                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD PROCESS APPTABD ENTRY                              *         
*                                                                     *         
* NTRY: P1 BYTE 0 = C'H' TO BUILD BFMELD FOR HEADLINE                 *         
*             1-3 = A(APPTABD ENTRY)                                  *         
*       P2        = A(RECORD)                                         *         
*           COLNO = CURRENT COLUMN NUMBER                             *         
*          LINENO = CURRENT LINE NUMBER                               *         
* EXIT:        CC = EQUAL IF ELEMENT REQUIRED                         *         
*           COLNO = COLUMN NUMBER FOR NEXT FIELD                      *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING BFMELD,BFMELD1                                                   
PRCAPP   NTR1  ,                                                                
         XR    R4,R4                                                            
         CLI   0(R1),C'H'                                                       
         BNE   *+6                                                              
         BCTR  R4,0                R4 IS NON-ZERO TO DO HEADLINE                
         XR    R2,R2                                                            
         ICM   R2,7,1(R1)                                                       
         USING APPTABD,R2                                                       
         L     R3,4(R1)                                                         
         USING BEDRECD,R3                                                       
         OC    APPTRTN,APPTRTN     CHECK FOR TEST ROUTINE                       
         BZ    *+12                                                             
         BAS   RE,PTRTN                                                         
         BNE   EXIT                                                             
*                                  INITIALIZE ELEMENT                           
         XC    BFMELD(BFMLNQ),BFMELD                                            
         MVI   BFMEL,BFMELQ                                                     
         MVI   BFMLN,BFMLNQ                                                     
         MVI   BFMTYPE,BFMTDATA                                                 
         MVC   BFMPANEL,APPPANEL                                                
         MVI   BFMALN,BFMALFTQ                                                  
*                                                                               
         TM    APPINDS,APPIAMT     TEST AMOUNT                                  
         BZ    *+12                                                             
         OI    BFMSTAT1,BFMSNUM                                                 
         MVI   BFMALN,BFMARGTQ                                                  
*                                                                               
         IC    RE,APPFWTH                                                       
         STC   RE,BFMWTH                                                        
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   BFMTEXT(0),BCSPACES                                              
         MVC   BFMWTH,APPFWTH                                                   
         MVI   BFMHGT,1                                                         
         MVC   BFMLEFT,COLNO                                                    
         MVC   BFMTOP,LINENO                                                    
*                                                                               
         LTR   R4,R4               TEST DOING HEADLINE                          
         BNZ   PAPP02                                                           
         BAS   RE,PORTN            NO JUST CALL OUTPUT ROUTINE                  
         B     PAPP10                                                           
*                                                                               
PAPP02   DS    0H                                                               
         MVI   BFMTYPE,BFMTFREE                                                 
         OI    BFMINDS1,BFMIUND                                                 
         MVI   BFMHGT,2                                                         
         LA    RF,BFMPANEL+L'BFMPANEL-2                                         
         CLI   0(RF),C' '          APPEND PANEL WITH H                          
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'H'                                                       
         XR    R5,R5               TRANSLATE HEADLINE                           
         IC    R5,BFMWTH                                                        
         MVI   BFMTEXT,AC#ESCL                                                  
         TM    APPINDS,APPIAMT                                                  
         BZ    *+10                                                             
         BCTR  R5,0                                                             
         MVI   BFMTEXT,AC#ESCR                                                  
         STC   R5,BFMTEXT+3                                                     
         MVC   BFMTEXT+1(L'APPDD),APPDD                                         
         GOTO1 VDICTAT,BOPARM,C'SL  ',BFMTEXT                                   
*                                                                               
PAPP10   DS    0H                                                               
         OC    BFMTLEN,BFMTLEN                                                  
         BNZ   PAPP12                                                           
         XR    RE,RE               FIND TEXT LENGTH                             
         IC    RE,BFMWTH                                                        
         LA    R5,BFMTEXT-1(RE)                                                 
         CLI   0(R5),C' '                                                       
         BH    *+10                                                             
         BCTR  R5,0                                                             
         BCT   RE,*-10                                                          
         STH   RE,BFMTLEN          SET TEXT AND ELEMENT LENGTH                  
         LA    RE,BFMLNQ(RE)                                                    
         STC   RE,BFMLN                                                         
*                                                                               
PAPP12   DS    0H                                                               
         IC    RE,COLNO            SET COLUMN NUMBER FOR NEXT FIELD             
         IC    RF,APPOWTH                                                       
         AR    RE,RF                                                            
         STC   RE,COLNO                                                         
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),BEDRECD,BFMELD,ADDEND                
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PRCAPPX  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
PTRTN    NTR1  ,                   GO TO TEST REQUIRED ROUTINE                  
         LH    RF,APPTRTN                                                       
         B     CLB65(RF)                                                        
         SPACE 1                                                                
PORTN    NTR1  ,                   GO TO OUTPUT ROUTINE                         
         LH    RF,APPORTN                                                       
         B     CLB65(RF)           BFMELD1 = BFMELD                             
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
ACCMST   DC    C'ACCMST '                                                       
ADDCODE  DC    C'ADD=CODE'                                                      
ADDEND   DC    C'ADD=END'                                                       
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
DMCB     EQU   BODMCB                                                           
DUB      EQU   BODUB1                                                           
WORK     EQU   BOWORK1                                                          
AMTWTHQ  EQU   14                  DEFAULT WIDTH OF AMOUNT                      
         EJECT                                                                  
***********************************************************************         
* MAIN BILL TOTALS                                                    *         
***********************************************************************         
         SPACE 1                                                                
MBTTABD  DSECT                                                                  
MBTPANEL DS    CL8                 PANEL KEYWORD                                
MBTROUT  DS    H                   DISPLACEMENT TO ROUTINE                      
MBTTABDL EQU   *-MBTTABD                                                        
         SPACE 1                                                                
CLB65    CSECT                                                                  
MBTTAB   DS    0XL(MBTTABDL)                                                    
*                                                                               
         DC    CL8'BT_NET'                                                      
         DC    AL2(MBTNET-CLB65)                                                
*                                                                               
         DC    CL8'BT_COM'                                                      
         DC    AL2(MBTCOM-CLB65)                                                
*                                                                               
         DC    CL8'BT_GROSS'                                                    
         DC    AL2(MBTGRS-CLB65)                                                
*                                                                               
         DC    CL8'BT_SURCH'                                                    
         DC    AL2(MBTSCH-CLB65)                                                
*                                                                               
         DC    CL8'BT_DISC'                                                     
         DC    AL2(MBTDSC-CLB65)                                                
*                                                                               
         DC    CL8'BT_VAT'                                                      
         DC    AL2(MBTVAT-CLB65)                                                
*                                                                               
         DC    CL8'BT_TRAX1'                                                    
         DC    AL2(MBTTRAX1-CLB65)                                              
*                                                                               
         DC    CL8'BT_TOTAL'                                                    
         DC    AL2(MBTINVT-CLB65)                                               
*                                                                               
         DC    CL8'BT_PPAY'                                                     
         DC    AL2(MBTPPAY-CLB65)                                               
*                                                                               
         DC    CL8'BT_CRAM'                                                     
         DC    AL2(MBTCRAM-CLB65)                                               
*                                                                               
         DC    CL8'BT_TRAX2'                                                    
         DC    AL2(MBTTRAX2-CLB65)                                              
*                                                                               
         DC    CL8'BT_VATA'                                                     
         DC    AL2(MBTVATA-CLB65)                                               
*                                                                               
MBTTABX  DS    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* VAT SUMMARY                                                         *         
***********************************************************************         
         SPACE 1                                                                
APPTABD  DSECT                                                                  
APPPANEL DS    CL8                 PANEL KEYWORD                                
APPINDS  DS    XL1                 INDICATOR BYTE                               
APPIAMT  EQU   X'80'               FIELD IS AMOUNT                              
         DS    XL1                 N/D                                          
APPDD    DS    AL2                 DATA DICTIONARY REF# FOR HEADLINE            
APPFWTH  DS    XL1                 FIELD WIDTH                                  
APPOWTH  DS    XL1                 OVERALL WIDTH (WIDTH TO NEXT FIELD)          
APPTRTN  DS    H                   DISP. TO TEST REQUIRED ROUTINE               
APPORTN  DS    H                   DISP. TO OUTPUT ROUTINE                      
APPTABL  EQU   *-APPTABD                                                        
         SPACE 1                                                                
CLB65    CSECT                                                                  
*                                                                               
VATTABH  DS    0XL(APPTABL)        DUMMY ENTRY FOR VAT ANALYSIS HEADING         
         DC    CL8'VA_'                                                         
         DC    AL1(0,0)                                                         
         DC    AL2(AC#VATAN)                                                    
         DC    AL1(20,0)                                                        
         DC    AL2(0,0)                                                         
*                                                                               
VATTAB   DS    0XL(APPTABL)                                                     
*                                                                               
         DC    CL8'VA_TYPE'                                                     
         DC    AL1(0,0)                                                         
         DC    AL2(AC#TYPE)                                                     
         DC    AL1(10,12)                                                       
         DC    AL2(VATTYPET-CLB65,VATTYPE-CLB65)                                
*                                                                               
         DC    CL8'VA_NETP'                                                     
         DC    AL1(APPIAMT,0)                                                   
         DC    AL2(AC#NETPV)                                                    
         DC    AL1(14,16)                                                       
         DC    AL2(0,VATNETP-CLB65)                                             
*                                                                               
         DC    CL8'VA_RATE'                                                     
         DC    AL1(APPIAMT,0)                                                   
         DC    AL2(AC#VATRT)                                                    
         DC    AL1(12,14)                                                       
         DC    AL2(0,VATRATE-CLB65)                                             
*                                                                               
         DC    CL8'VA_AMT'                                                      
         DC    AL1(APPIAMT,0)                                                   
         DC    AL2(AC#VATAM)                                                    
         DC    AL1(14,16)                                                       
         DC    AL2(0,VATAMT-CLB65)                                              
*                                                                               
VATTABX  DS    AL1(EOT)                                                         
         EJECT                                                                  
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
* ACCLBLINK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBLINK                                                      
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
BTWORKD  DSECT                                                                  
*                                                                               
APBLK    DS    A                   A(PBLKD)                                     
*                                                                               
BTBILNUM DS    CL6                 BILL NUMBER                                  
BTNET    DS    PL8                 NET                                          
BTCOM    DS    PL8                 COMMISSION                                   
BTSRC    DS    PL8                 SURCHARGE AMOUNT                             
BTSRCPC  DS    PL8                 SURCHARGE PERCENT                            
BTDSC    DS    PL8                 DISCOUNT AMOUNT                              
BTDSCPC  DS    PL8                 DISCOUNT PERCENT                             
BTVAT    DS    PL8                 VAT AMOUNT                                   
BTVATAGY DS    PL8                 VAT AMOUNT (AGENCY TOTAL)                    
*                                                                               
BTFMTHDR DS    XL42                FORMAT RECORD KEY                            
SVBOFEL  DS    XL255               SAVED FORMAT OPTIONS ELEMENT                 
SVBTTEL  DS    XL255               SAVED BILL TOTALS ELEMENT                    
*                                                                               
BTINDS   DS    XL1                 INDICATOR BYTE                               
BTIADD   EQU   X'80'               ADD RECORD                                   
INVTOT   DS    PL8                                                              
*                                  * FMTAMT ROUTINE *                           
FAPARMS  DS    0XL8                                                             
FAPINDS  DS    XL1                 INDICATOR BYTE                               
FAPIAGY  EQU   X'80'               USE AGENCY CURRENCY, NOT BILLING             
FAPICODE EQU   X'40'               PREFIX AMOUNT WITH CURRENCY CODE             
FAPABFM  DS    AL3                 A(BFMELD)                                    
FAPAAMT  DS    AL4                 A(AMOUNT)                                    
FAPL8    DS    PL8                                                              
FACUR    DS    XL(L'CSCURBIL)                                                   
*                                                                               
LINENO   DS    XL1                 LINE NUMBER                                  
COLNO    DS    XL1                                                              
ABFPEL   DS    AL4                 A(BFP ELEMENT)                               
BFMELD1  DS    XL255               BFMELD BUILD AREA 1                          
BFMELD2  DS    XL255               BFMELD BUILD AREA 2                          
BFMELD3  DS    XL255               BFMELD BUILD AREA 3                          
*                                                                               
         DS    (OVERWRKL-(*-BTWORKD))X                                          
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'098ACCLB65   08/16/00'                                      
         END                                                                    
