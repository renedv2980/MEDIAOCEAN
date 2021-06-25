*          DATA SET ACCLB63    AT LEVEL 009 AS OF 08/16/00                      
*PHASE T62163A                                                                  
*&&      SET   NOP=N                                                            
CLB63    TITLE '- PC COMMS - AUTOFORM BODY OF BILL'                             
CLB63    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB63**,R8,CLEAR=YES,RR=RE                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK                                                      
         USING AFWORKD,RC                                                       
*                                                                               
         BAS   RE,SAVFMT           SAVE FORMAT DATA                             
         BNE   EXIT                                                             
         BAS   RE,BLDBIL           BUILD BILL TSAR RECORDS                      
         BNE   EXIT                                                             
         BAS   RE,BLDFIL           BUILD BILL FILE RECORDS                      
         BNE   EXIT                                                             
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
* ROUTINE TO SAVE FORMAT DATA                                         *         
***********************************************************************         
         SPACE 1                                                                
SAVFMT   NTR1  ,                                                                
         USING BFMRECD,IOKEY       READ THROUGH RECORDS FOR FORMAT              
         L     RF,ALINK                                                         
         MVC   BFMKEY,BEWFMTKY-LINKD(RF)                                        
         LA    R1,IOHIGH+IOACCDIR+IO2                                           
         B     *+8                                                              
SFMT02   LA    R1,IOSEQ+IOACCDIR+IO2                                            
         GOTO1 AIO                                                              
         BNE   SAVFMTX                                                          
         CLC   BFMKEY(BFMKLVL-BFMKEY),IOKEYSAV                                  
         BNE   SAVFMTX                                                          
         CLC   BFMKWHER,=AL2(BFMKWBDQ)                                          
         BNE   SFMT02                                                           
*                                                                               
         CLI   BFMKSEQ,0           TEST RECORD IS CONTINUATION                  
         BNE   SFMT04                                                           
         TM    AFINDS,AFICONT                                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AFBFMDA,BFMKDA      SAVE DISK ADDRESS FOR SORT KEY               
         B     SFMT06                                                           
*                                                                               
SFMT04   DS    0H                                                               
         TM    AFINDS,AFICONT                                                   
         BO    *+6                                                              
         DC    H'0'                                                             
         CLC   BFMKEY(BFMKSEQ-BFMKEY),IOKEYSAV                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IODAOVER,BFMKDA                                                  
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2             COPY CONTINUED RECORD TO ORIGINAL            
TO       USING BFMRECD,R2                                                       
         L     R3,AIO1                                                          
FROM     USING BFMRECD,R3                                                       
*                                                                               
         LH    R1,FROM.BFMRLEN     R1 = LENGTH TO COPY                          
         AHI   R1,-(BFMRFST-BFMRECD)                                            
         LH    RF,TO.BFMRLEN                                                    
         LA    RE,TO.BFMRECD(RF)   RE = DESTINATION OF COPY                     
         AR    RF,R1                                                            
         STH   RF,TO.BFMRLEN       SET NEW LENGTH OF RECORD                     
         LA    R0,FROM.BFMRFST     R0 = SOURCE OF COPY                          
         LR    RF,R1                                                            
         MVCL  RE,R0               COPY RECORD                                  
         DROP  FROM,TO                                                          
*                                                                               
SFMT06   DS    0H                                                               
         TM    BFMKSTAT,BFMSCONT   TEST RECORD IS CONTINUED                     
         BZ    *+12                                                             
         OI    AFINDS,AFICONT                                                   
         B     SFMT02                                                           
         NI    AFINDS,FF-AFICONT                                                
*                                                                               
         OC    BFMKSWHR,BFMKSWHR   TEST SECTION MASTER RECORD                   
         BNZ   SFMT08                                                           
         GOTO1 SAVFLT,BOPARM,AIO2  YES - SAVE SECTION FILTERS                   
         B     SFMT02                                                           
SFMT08   DS    0H                  NO - SAVE SORT DATA                          
         GOTO1 SAVSRT,BOPARM,AIO2                                               
         B     SFMT02                                                           
*                                                                               
SAVFMTX  DS    0H                                                               
         OC    AFLVLCNT,AFLVLCNT                                                
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$RECNF)                                           
         B     EXITN                                                            
*                                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SAVE SECTION FILTER DATA                                 *         
*                                                                     *         
* NTRY: P1 = A(SECTION MASTER RECRD)                                  *         
***********************************************************************         
         SPACE 1                                                                
SAVFLT   NTR1  ,                                                                
         L     R2,0(R1)                                                         
         USING BFMRECD,R2                                                       
*                                                                               
         LA    RF,AFLVLLST         ADD TO BILL LEVEL LIST IF NEC                
         LA    R0,AFLVLMAX                                                      
SFLT02   CLC   BFMKLVL,0(RF)                                                    
         BE    SFLT10                                                           
         OC    0(L'BFMKLVL,RF),0(RF)                                            
         BZ    SFLT04                                                           
         LA    RF,L'BFMKLVL(RF)                                                 
         BCT   R0,SFLT02                                                        
         DC    H'0'                                                             
SFLT04   MVC   0(L'BFMKLVL,RF),BFMKLVL                                          
         LH    RE,AFLVLCNT                                                      
         LA    RE,1(RE)                                                         
         STH   RE,AFLVLCNT                                                      
*                                                                               
SFLT10   L     R5,ALSVALS                                                       
         USING FLSTD,R5                                                         
         XC    FLKEY,FLKEY                                                      
         MVI   FLKTYP,FLKTYPQ                                                   
         MVC   FLKLVL,BFMKLVL                                                   
         MVC   FLKSECT,BFMKSECT                                                 
         ZAP   FLNET,BCPZERO                                                    
         ZAP   FLCOM,BCPZERO                                                    
         LA    R4,FLDATA           R4 = A(COPY AREA)                            
         USING SFELD,R4                                                         
         XC    SFELD(SFLNQ),SFELD                                               
*                                                                               
         LA    R3,BFMRFST                                                       
         USING BSDELD,R3                                                        
         XR    RF,RF                                                            
SFLT12   CLI   BSDEL,0                                                          
         BE    SAVFLTX                                                          
         CLI   BSDEL,BSDELQ                                                     
         BNE   SFLT18                                                           
         CLI   BSDSTYP,BSDSALL     TEST CATCH ALL                               
         BNE   *+12                                                             
         MVI   FLKCALL,FLKCALLQ                                                 
         B     SAVFLTX                                                          
*                                                                               
         CLC   SFEL,BSDSTYP        TEST CHANGE OF TYPE                          
         BE    SFLT14                                                           
         IC    RF,SFLN             YES BUMP R4 FOR NEXT ELEMENT                 
         AR    R4,RF                                                            
         MVC   SFEL,BSDSTYP                                                     
         MVI   SFLN,SFLNQ                                                       
         MVI   SFNUM,0                                                          
*                                                                               
SFLT14   DS    0H                                                               
         IC    RF,BSDLN                                                         
         SH    RF,=Y(BSDLN1Q)                                                   
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    RF,*+4                                                           
         MVC   SFDATA(0),BSDDATA                                                
         IC    RE,SFLN                                                          
         AR    RE,RF                                                            
         STC   RE,SFLN                                                          
         IC    RE,SFNUM                                                         
         LA    RE,1(RE)                                                         
         STC   RE,SFNUM                                                         
*                                                                               
SFLT18   DS    0H                                                               
         IC    RF,BSDLN                                                         
         BXH   R3,RF,SFLT12                                                     
         DROP  R3                                                               
*                                                                               
SAVFLTX  DS    0H                                                               
         IC    RF,SFLN                                                          
         AR    R4,RF                                                            
         MVI   0(R4),0                                                          
         DROP  R4                                                               
*                                                                               
         LA    RE,FLREC                                                         
         SR    R4,RE                                                            
         LA    R4,1(R4)                                                         
         CHI   R4,FLMAXLNQ                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         STH   R4,FLRLEN                                                        
         GOTO1 ATSARIO,TSAADD                                                   
*                                                                               
         B     EXITY                                                            
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SAVE SECTION SORT DATA                                   *         
*                                                                     *         
* NTRY: P1 = A(SECTION PARAGRAPH RECORD)                              *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
SAVSRT   NTR1  ,                                                                
         L     R2,0(R1)                                                         
         USING BFMRECD,R2                                                       
*                                                                               
         LA    R3,BFMRFST          TEST ANY ITEMS IN RECORD                     
         USING BFPELD,R3                                                        
         XR    RF,RF                                                            
SSRT02   CLI   BFPEL,BFPELQ                                                     
         BE    SSRT04                                                           
         CLI   BFPEL,0                                                          
         BE    EXIT                                                             
         IC    RF,BFPLN                                                         
         BXH   R3,RF,SSRT04                                                     
SSRT04   CLI   BFPHGTMN,0          MIN HEIGHT = 0 MEANS NO ITEMS                
         BE    EXIT                                                             
         DROP  R3                                                               
*                                                                               
SSRT10   DS    0H                                                               
         L     R5,ALSVALS          INITIALIZE TSAR RECORD                       
         USING PLSTD,R5                                                         
         XC    PLKEY,PLKEY                                                      
         MVI   PLKTYP,PLKSRTQ                                                   
         MVC   PLKLVL,BFMKLVL                                                   
         MVC   PLKSECT,BFMKSECT                                                 
         MVC   PLKSWHR,BFMKSWHR                                                 
         MVC   PLKBFMDA,AFBFMDA                                                 
*                                                                               
         XC    BOWORK1,BOWORK1     INITIALIZE SORTED DATA LIST                  
SRT      USING SRTELD,BOWORK1                                                   
         MVI   SRT.SRTEL,SRTSRTQ                                                
         LA    R3,BFMRFST                                                       
         USING BFSELD,R3                                                        
         XR    RF,RF                                                            
SSRT12   CLI   BFSEL,0                                                          
         BE    SSRT20                                                           
         CLI   BFSEL,BFSELQ                                                     
         BNE   SSRT18                                                           
         MVC   PLKSSUB,BFSLVL                                                   
         XR    R0,R0                                                            
         IC    R0,BFSLN                                                         
         AHI   R0,-(BFSBODYS-BFSELD)                                            
         BZ    SSRT20                                                           
         LA    R4,BFSBODYS                                                      
SSRT14   GOTO1 ADDSRT,BOPARM,(R4),SRT.SRTELD                                    
         LA    R4,L'BFMBODY(R4)                                                 
         BCT   R0,SSRT14                                                        
         B     SSRT20                                                           
*                                                                               
SSRT18   IC    RF,BFSLN                                                         
         BXH   R3,RF,SSRT12                                                     
*                                                                               
SSRT20   XC    BOWORK2,BOWORK2     INITIALIZE TOTAL FIELDS LIST                 
TOT      USING SRTELD,BOWORK2                                                   
         MVI   TOT.SRTEL,SRTTOTQ                                                
         CLC   BFMKSWHR,=AL2(BFMKSPRQ)                                          
         BL    SSRT21              IF NOT A HEADING PARAGRPAPH                  
         MVI   TOT.SRTNUM,3        ADD NET/COMMISSION/GROSS                     
         MVI   TOT.SRTLIST+00,BLFFNETQ                                          
         MVI   TOT.SRTLIST+01,BLFFCOMQ                                          
         MVI   TOT.SRTLIST+02,BLFFGRSQ                                          
*                                                                               
SSRT21   LA    R3,BFMRFST                                                       
         USING BFMELD,R3                                                        
SSRT22   DS    0H                                                               
         CLI   BFMEL,0                                                          
         BE    SSRT30                                                           
         CLI   BFMEL,BFMELQ                                                     
         BNE   SSRT28                                                           
         CLI   BFMTYPE,BFMTDATA                                                 
         BNE   SSRT28                                                           
         CLI   BFMBODY,0                                                        
         BE    SSRT28                                                           
         GOTO1 GETFLD,BOPARM,('R4',BFMBODY)                                     
         USING FLDTABD,R4                                                       
*                                                                               
         TM    FLDINDS,FLDITOT     TEST TOTALLED AMOUNT                         
         BZ    SSRT28                                                           
         GOTO1 ADDSRT,BOPARM,BFMBODY,TOT.SRTELD                                 
         DROP  R4                                                               
*                                                                               
SSRT28   DS    0H                                                               
         XR    RF,RF                                                            
         IC    RF,BFMLN                                                         
         BXH   R3,RF,SSRT22                                                     
         DROP  R3                                                               
*                                                                               
SSRT30   DS    0H                                                               
         LA    R3,PLDATA           ADD LIST FOR FIELD DATA                      
         XR    RE,RE                                                            
         ICM   RE,1,SRT.SRTNUM                                                  
         BZ    SSRT32                                                           
         LA    RE,SRTLNQ(RE)                                                    
         STC   RE,SRT.SRTLN                                                     
         EX    RE,*+4                                                           
         MVC   0(0,R3),SRT.SRTELD                                               
*                                                                               
SSRT32   AR    R3,RE               ADD BUCKET AMOUNT LIST                       
         ICM   RE,1,TOT.SRTNUM                                                  
         BZ    SSRT34                                                           
         LA    RE,SRTLNQ(RE)                                                    
         STC   RE,TOT.SRTLN                                                     
         EX    RE,*+4                                                           
         MVC   0(0,R3),TOT.SRTELD                                               
         AR    R3,RE                                                            
*                                                                               
SSRT34   MVI   0(R3),0                                                          
         LA    R3,1(R3)                                                         
         SR    R3,R5                                                            
         STH   R3,PLRLEN                                                        
         GOTO1 ATSARIO,TSAADD                                                   
         B     EXIT                                                             
         POP   USING                                                            
         SPACE 1                                                                
***********************************************************************         
* ADD BODY FIELD TO SRTELD                                            *         
*                                                                     *         
* NTRY: P1 = A(BODY FIELD)                                            *         
*       P2 = A(SORT ELEMENT)                                          *         
***********************************************************************         
         SPACE 1                                                                
ADDSRT   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING SRTELD,R3                                                        
         LA    RF,SRTLIST                                                       
         XR    R0,R0                                                            
         ICM   R0,1,SRTNUM         TEST ALREADY IN LIST                         
         BZ    ASRT04                                                           
ASRT02   CLC   0(L'BFMBODY,RF),0(R2)                                            
         BE    ADDSRTX                                                          
         LA    RF,L'BFMBODY(RF)                                                 
         BCT   R0,ASRT02                                                        
ASRT04   MVC   0(L'BFMBODY,RF),0(R2)                                            
         IC    RE,SRTNUM                                                        
         LA    RE,1(RE)                                                         
         STC   RE,SRTNUM                                                        
*                                                                               
ADDSRTX  B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD PARAGRAPH TSAR RECORDS                             *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BLDBIL   NTR1  ,                                                                
         L     R7,ALINK                                                         
         USING LINKD,R7                                                         
         ZAP   AFTOTNET,BCPZERO    INITIALIZE BILL TOTALS                       
         ZAP   AFTOTCOM,BCPZERO                                                 
K        USING TRNRECD,IOKEY                                                    
         MVC   K.TRNKEY,BCSPACES                                                
         MVC   K.TRNKCPY,CUABIN                                                 
         MVC   K.TRNKUNT(L'BCCPYPRD),BCCPYPRD                                   
         MVC   K.TRNKACT,BEWJOB                                                 
         MVI   AFADVSEQ,0          SET NOT PROCESSING ADVANCES                  
*                                                                               
         LA    R1,IOHIGH+IOACCDIR+IO1                                           
         B     *+8                                                              
BBIL02   LA    R1,IOSEQ+IOACCDIR+IO1                                            
         GOTO1 AIO                                                              
         BNE   BBIL10                                                           
         CLC   K.TRNKEY(TRNKWORK-TRNKEY),IOKEYSAV                               
         BNE   BBIL10                                                           
         CLC   K.TRNKWORK,=C'99'                                                
         BE    BBIL10                                                           
         NI    AFINDS,FF-AFIBXSEQ  SET READ SEQUENCE NOT BROKEN                 
*                                                                               
         CLC   K.TRNKDATE,BCSPACES ENSURE TRANSACTION RECORD                    
         BNH   BBIL02                                                           
         TM    K.TRNKSTAT,TRNSREVS TEST FOR REVERSAL                            
         BO    BBIL02                                                           
* ?? TEMP TAKEN OUT                                                             
* ??     TM    K.TRNKSTA2,TRNSBILP TEST FOR PENDING ALLOCATOIN                  
* ??     BZ    BBIL02                                                           
*                                                                               
         TM    K.TRNKSTAT,TRNSDRFT TEST DRAFT                                   
         BZ    BBIL04                                                           
         CLI   K.TRNKSTYP,99       FOR NOW, ALLOW 99S                           
         BNE   BBIL02                                                           
*                                                                               
BBIL04   DS    0H                                                               
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 PRCTRN,BOPARM,AIO1                                               
*                                                                               
         TM    AFINDS,AFIBXSEQ     TEST READ SEQUENCE BROKEN                    
         BZ    BBIL02                                                           
         L     RF,AIO1             RE-ESTABLISH READ SEQUENCE                   
         MVC   IOKEY(L'TRNKEY),0(RF)                                            
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     BBIL02                                                           
         DROP  K                                                                
*                                                                               
BBIL10   DS    0H                  TEST FOR ANY ADVANCES                        
         CLI   BEWBLH+(BLHNADV-BLHELD),0                                        
         BE    BLDBILX                                                          
K        USING BEDRECD,AFADVKEY                                                 
         MVC   K.BEDKEY,BEWHDRKY                                                
         MVC   K.BEDKLVL,=AL2(BEDKLADV)                                         
BBIL12   MVC   IOKEY(L'BEDKEY),K.BEDKEY                                         
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BNE   BBIL20                                                           
         CLC   IOKEY(BEDKSEQ-BEDKEY),IOKEYSAV                                   
         BNE   BBIL20                                                           
         IC    RE,K.BEDKSEQ        ADVANCE SEQUENCE NO. = BFMKSEQ + 1           
         LA    RE,1(RE)                                                         
         STC   RE,AFADVSEQ                                                      
         CLI   AFADVSEQ,0                                                       
         BE    BBIL20                                                           
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
*                                  DISGUISE ADVANCE AS TRANSACTION              
         GOTO1 AADVANCE,BOPARM,=C'CNVTRANS',AIO1                                
         GOTO1 PRCTRN,BOPARM,AIO1                                               
*                                                                               
BBIL18   MVC   K.BEDKSEQ,AFADVSEQ  INCREMENT SEQUENCE NUMBER                    
         B     BBIL12                                                           
         DROP  K                                                                
BBIL20   DS    0H                                                               
*                                                                               
BLDBILX  DS    0H                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTION                                                 *         
*                                                                     *         
* NTRY: P1 = A(TRANSACTION RECORD)                                    *         
***********************************************************************         
         SPACE 1                                                                
PRCTRN   NTR1  ,                                                                
         L     R2,0(R1)                                                         
         USING TRNRECD,R2                                                       
*                                                                               
         L     RF,ALINK                                                         
         TM    BEWINDS-LINKD(RF),BEWISEC TEST BILLING IN SECONDARY CURR         
         BZ    PTRN02                                                           
         GOTO1 VTOBACCO,BOPARM,('TOBAACVS',TOBCUR),TRNRECD,ACOM,0,0,0           
*                                                                               
PTRN02   DS    0H                                                               
         GOTO1 GETTRN,BOPARM,TRNRECD                                            
         BNE   PRCTRNX                                                          
*                                                                               
         LH    R0,AFLVLCNT         BUILD PARAS FOR EACH LEVEL OF BILL           
         LA    R3,AFLVLLST                                                      
PTRN04   DS    0H                                                               
         GOTO1 GETSECT,BOPARM,TRNRECD,(R3),BOBYTE1                              
         BNE   PTRN06              ??                                           
         GOTO1 BLDPAR,(R1),TRNRECD,(R3),BOBYTE1                                 
PTRN06   LA    R3,L'AFLVLLST(R3)                                                
         BCT   R0,PTRN04                                                        
*                                                                               
PRCTRNX  DS    0H                                                               
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT VALUES FROM TRANSACTION RECORD                   *         
*                                                                     *         
* NTRY: P1 = A(RECORD)                                                *         
* EXIT: CC = EQUAL IF RECORD OKAY                                     *         
***********************************************************************         
         SPACE 1                                                                
GETTRN   NTR1  ,                                                                
         L     R2,0(R1)                                                         
         USING TRNRECD,R2                                                       
         LA    R3,TRNRFST                                                       
         USING PTAELD,R3                                                        
         XR    RF,RF                                                            
*                                                                               
GTRN02   CLI   PTAEL,0             FIND PENDING BILL PTAELD                     
         BE    GETTRNN                                                          
         CLI   PTAEL,PTAELQ                                                     
         BNE   GTRN08                                                           
         CLI   PTATYPE,PTATRAL                                                  
         BNE   GTRN08                                                           
         TM    PTASTAT1,PTASPEND                                                
         BZ    GTRN08                                                           
         CLC   PTACUR,CSBILCUR                                                  
         BNE   GTRN08                                                           
         CP    PTANET,BCPZERO                                                   
         BNE   GTRN10                                                           
         CP    PTARCOM,BCPZERO                                                  
         BNE   GTRN10                                                           
GTRN08   IC    RF,PTALN                                                         
         BXH   R3,RF,GTRN02                                                     
*                                                                               
GTRN10   ZAP   AFTRNNET,PTANET                                                  
         ZAP   AFTRNCOM,PTARCOM                                                 
         L     RF,ALINK                                                         
         TM    BEWINDS-LINKD(RF),BEWIFRGN                                       
         BZ    *+16                                                             
         ZAP   AFTRNNET,PTANETF                                                 
         ZAP   AFTRNCOM,PTARFCOM                                                
         ZAP   AFTRNGRS,AFTRNNET                                                
         AP    AFTRNGRS,AFTRNCOM                                                
         LH    RF,PTAHOURS                                                      
         CVD   RF,AFTRNHRS                                                      
         ZAP   AFTRNCRT,BCPZERO                                                 
         OC    PTARCORT,PTARCORT                                                
         BZ    *+10                                                             
         ZAP   AFTRNCRT,PTARCORT                                                
         DROP  R3                                                               
*                                                                               
         ZAP   AFTRNRAT,BCPZERO                                                 
         LA    R3,TRNRFST                                                       
         USING PRTELD,R3                                                        
         XR    RF,RF                                                            
GTRN12   CLI   PRTEL,0                                                          
         BE    GTRN14                                                           
         CLI   PRTEL,PRTELQ                                                     
         BE    *+12                                                             
         IC    RF,PRTLN                                                         
         BXH   R3,RF,GTRN12                                                     
         ZAP   AFTRNRAT,PRTRATE    ?? WHAT ABOUT BT8 AND SCITCRAT               
         DROP  R3                                                               
*                                                                               
GTRN14   AP    AFTOTNET,AFTRNNET   UPDATE BILL TOTALS                           
         AP    AFTOTCOM,AFTRNCOM                                                
*                                                                               
         CLC   AFCAC,TRNKULC       TEST CHANGE IN CONTRA-ACCOUNT                
         BE    GTRN20                                                           
         OI    AFINDS,AFIBXSEQ                                                  
         MVC   AFCAC,TRNKULC                                                    
         MVI   AFCACLEN,0                                                       
         MVC   AFCACNAM,BCSPACES   GET CONTRA-ACCOUNT NAME                      
         LA    R4,IOKEY                                                         
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,AFCAC                                                    
         GOTO1 AIO,IOREAD+IOACCDIR+IO2                                          
         BNE   GTRN20                                                           
         GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BNE   GTRN20                                                           
         L     R4,AIO2                                                          
         LA    R3,ACTRFST                                                       
         USING NAMELD,R3                                                        
         XR    RF,RF                                                            
GTRN16   CLI   NAMEL,0                                                          
         BE    GTRN20                                                           
         IC    RF,NAMLN                                                         
         CLI   NAMEL,NAMELQ                                                     
         BE    *+8                                                              
         BXH   R3,RF,GTRN16                                                     
         AHI   RF,-NAMLN1Q                                                      
         BNP   GTRN20                                                           
         STC   RF,AFCACLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   AFCACNAM(0),NAMEREC                                              
         DROP  R4,R3                                                            
*                                                                               
GTRN20   DS    0H                                                               
         CLC   AFWC,TRNKWORK       TEST CHANGE IN WORKCODE                      
         BE    GETTRNY                                                          
         OI    AFINDS,AFIBXSEQ                                                  
         MVC   AFWC,TRNKWORK                                                    
         MVI   AFWCGRP,0                                                        
         MVI   AFWCNAML,0                                                       
         MVC   AFWCNAME,BCSPACES                                                
         MVI   AFWCTYPE,BSDTIMEQ                                                
         GOTO1 AGETOPT,BOPARM,TRNRECD                                           
*                                                                               
         LA    R4,IOKEY                                                         
         USING WCORECD,R4                                                       
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   WCOKWRK,TRNKWORK                                                 
         GOTO1 AIO,IOREAD+IOACCDIR+IO2                                          
         BNE   GTRN40                                                           
         GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO2                                                          
*                                                                               
         LA    R3,WCORFST                                                       
         USING WCOELD,R3                                                        
         CLI   WCOEL,WCOELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    WCOSTAT,WCOSHCOE                                                 
         BO    *+8                                                              
         MVI   AFWCTYPE,BSDCOSTQ                                                
         MVC   AFWCGRP,WCOGRP                                                   
         LA    R4,WCODESC          R4 = A(NAME)                                 
         LA    R5,L'WCODESC        R5 = L(NAME)                                 
         DROP  R3                                                               
*                                                                               
                                                                                
         XR    RF,RF               FIND THE NAME WE WANT                        
GTRN22   CLI   0(R3),0                                                          
         BE    GTRN30                                                           
*                                                                               
         USING NAMELD,R3                                                        
         CLI   NAMEL,NAMELQ        TEST FOR NAME ELEMENT                        
         BNE   GTRN24                                                           
         LA    R4,NAMEREC                                                       
         IC    R5,NAMLN                                                         
         AHI   R5,-(NAMEREC-NAMELD)                                             
         B     GTRN28                                                           
*                                                                               
         USING XNMELD,R3                                                        
GTRN24   CLI   XNMEL,XNMELQ        TEST FOREIGN LANGUAGE NAME                   
         BNE   GTRN26                                                           
         CLI   CSFMLANG,0          TEST FORMATTING IN FOREIGN LANGUAGE          
         BE    GTRN28              NO                                           
         MVC   BOBYTE1,CSFMLANG    SET FORMAT LANGUAGE                          
         CLI   CSFMLANG,LANGEUK    TEST IF ENGLISH                              
         BNE   *+8                                                              
         MVI   BOBYTE1,LANGENG     SET DEFAULT ENGLISH                          
         CLC   XNMSTAT,BCBYTE1                                                  
         BNE   GTRN28                                                           
         LA    R4,XNMSUBN                                                       
         IC    R5,XNMSUBL                                                       
         B     GTRN28                                                           
*                                                                               
         USING FFTELD,R3                                                        
GTRN26   CLI   FFTEL,FFTELQ        TEST FOR FREE-FORM TEXT ELEMENT              
         BNE   GTRN28                                                           
         CLI   FFTTYPE,FFTTFREE                                                 
         BNE   GTRN28                                                           
         CLI   FFTSEQ,0                                                         
         BNE   GTRN28                                                           
         CLI   FFTDATA,C'*'        TWINKLE MEANS DO NOT USE                     
         BE    GTRN28                                                           
         LA    R4,FFTDATA                                                       
         IC    R5,FFTDLEN                                                       
         B     GTRN30                                                           
         DROP  R3                                                               
*                                                                               
GTRN28   IC    RF,1(R3)                                                         
         BXH   R3,RF,GTRN22                                                     
*                                                                               
GTRN30   STC   R5,AFWCNAML         COPY WHATEVER NAME FOUND                     
         MVC   AFWCNAME,BCSPACES                                                
         LTR   R5,R5                                                            
         BZ    GTRN40                                                           
         BCTR  R5,0                                                             
         EX    R5,*+4                                                           
         MVC   AFWCNAME(0),0(R4)                                                
*                                                                               
GTRN40   DS    0H                  RE-ESTABLISH READ SEQUENCE                   
*                                                                               
GETTRNY  B     EXITY                                                            
*                                                                               
GETTRNN  B     EXITN                                                            
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIND SECTION FOR TRANSACTION RECORD                      *         
*                                                                     *         
* NTRY: P1 = A(RECORD)                                                *         
*       P2 = A(LEVEL OF BILL)                                         *         
*       P3 = A(AREA FOR SECTION NUMBER)                               *         
* EXIT: CC = EQUAL IF FOUND                                           *         
***********************************************************************         
         SPACE 1                                                                
GETSECT  NTR1  ,                                                                
         LM    R2,R4,0(R1)                                                      
         USING TRNRECD,R2                                                       
         L     R5,ALSVALS                                                       
         USING FLSTD,R5                                                         
         XC    FLKEY,FLKEY                                                      
         MVI   FLKTYP,FLKTYPQ                                                   
         MVC   FLKLVL,0(R3)                                                     
*                                                                               
         LA    R1,TSARDH                                                        
         B     *+8                                                              
GSECT02  LA    R1,TSANXT                                                        
         GOTO1 ATSARIO                                                          
         BL    GETSECTN                                                         
         CLI   FLKTYP,FLKTYPQ                                                   
         BNE   GETSECTN                                                         
         CLC   FLKLVL,0(R3)                                                     
         BNE   GETSECTN                                                         
         GOTO1 TSTSECT,BOPARM,TRNRECD,FLSTD                                     
         BE    GETSECTY                                                         
         B     GSECT02                                                          
*                                                                               
GETSECTN DS    0H                                                               
         MVI   0(R4),0             ?? DUMMY CATCH-ALL SECTION ??                
         B     EXITN                                                            
*                                                                               
GETSECTY DS    0H                                                               
         AP    FLNET,AFTRNNET      UPDATE SECTION NET/COMM TOTALS               
         AP    FLCOM,AFTRNCOM                                                   
         GOTO1 ATSARIO,TSAPUT                                                   
         MVC   0(L'FLKSECT,R4),FLKSECT                                          
         B     EXITY                                                            
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST TRANSACTION IS IN SECTION                           *         
*                                                                     *         
* NTRY: P1 = A(RECORD)                                                *         
*       P2 = A(TSAR RECORD FOR SECTION)                               *         
* EXIT: CC = EQUAL IF TRANSACTION IN SECTION                          *         
***********************************************************************         
         SPACE 1                                                                
TSTSECT  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING TRNRECD,R2                                                       
         USING FLSTD,R3                                                         
         CLI   FLKCALL,FLKCALLQ    TEST IS CATCH ALL SECTION                    
         BE    TSTSECTY                                                         
         CLI   FLDATA,0            TEST SECTION HAS NO FILTERS                  
         BE    TSTSECTN                                                         
*                                                                               
         LA    R4,FLDATA                                                        
         USING SFELD,R4                                                         
TSECT02  CLI   0(R4),0                                                          
         BE    TSTSECTY                                                         
*                                                                               
         LA    R5,FLTTAB                                                        
         USING FLTTABD,R5                                                       
TSECT03  CLC   FLTTYPE,SFEL                                                     
         BE    TSECT04                                                          
         LA    R5,FLTTABL(R5)                                                   
         CLI   FLTTYPE,0                                                        
         BNE   TSECT03                                                          
         DC    H'0'                                                             
*                                                                               
TSECT04  DS    0H                                                               
         XR    R0,R0                                                            
         IC    R0,SFNUM                                                         
         LA    R6,SFDATA                                                        
         XR    RF,RF                                                            
         IC    RF,FLTLEN                                                        
TSECT06  DS    0H                                                               
         BAS   RE,TSTFLT                                                        
         BE    TSECT08                                                          
         AR    R6,RF                                                            
         BCT   R0,TSECT06                                                       
         B     TSTSECTN                                                         
*                                                                               
TSECT08  DS    0H                                                               
         XR    RF,RF                                                            
         IC    RF,SFLN                                                          
         BXH   R4,RF,TSECT02                                                    
*                                                                               
TSTSECTN B     EXITN                                                            
*                                                                               
TSTSECTY B     EXITY                                                            
         SPACE 1                                                                
TSTFLT   NTR1  ,                   ** TEST DATA **                              
         LH    RF,FLTROUT                                                       
         B     CLB63(RF)                                                        
*                                                                               
TSTWCT   CLC   AFWCTYPE,0(R6)                                                   
         B     EXIT                                                             
*                                                                               
TSTWCG   CLC   AFWCGRP,0(R6)                                                    
         B     EXIT                                                             
*                                                                               
TSTWC    CLC   TRNKWORK,0(R6)                                                   
         B     EXIT                                                             
*                                                                               
TSTTRN   CLC   TRNRSTYP,0(R6)                                                   
         B     EXIT                                                             
*                                                                               
TSTCAC   CLC   TRNKULC,0(R6)                                                    
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD PARAGPRAH RECORD                                   *         
*                                                                     *         
* NTRY: P1 = A(RECORD)                                                *         
*       P2 = A(LEVEL OF BILL)                                         *         
*       P3 = A(SECTION NUMBER)                                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BLDPAR   NTR1  ,                                                                
         LM    R2,R4,0(R1)                                                      
         USING TRNRECD,R2                                                       
         L     R5,ALSVALS                                                       
         USING PLSTD,R5                                                         
         XC    PLKEY,PLKEY                                                      
         MVI   PLKTYP,PLKSRTQ                                                   
         MVC   PLKLVL,0(R3)                                                     
         MVC   PLKSECT,0(R4)                                                    
         MVC   BOWORK1(L'PLKEY),PLKEY                                           
*                                                                               
         L     R4,AIO2                                                          
PAR      USING PLSTD,R4                                                         
         LA    R1,TSARDH                                                        
         B     *+8                                                              
BPAR02   LA    R1,TSANXT                                                        
         GOTO1 ATSARIO                                                          
         BL    BPAR20                                                           
         CLC   PLKEY(PLKSWHR-PLKEY),BOWORK1                                     
         BNE   BPAR20                                                           
*                                                                               
         MVC   PAR.PLKEY,PLKEY     INITIALIZE TSAR PARAGRAPH RECORD             
         MVI   PAR.PLKTYP,PLKPARQ                                               
         XC    PAR.PLTRNCNT,PAR.PLTRNCNT                                        
         XC    PAR.PLADVCNT,PAR.PLADVCNT                                        
         MVC   PAR.PL1STADV,AFADVSEQ                                            
         CLI   PAR.PL1STADV,0                                                   
         BE    *+14                                                             
         MVC   PAR.PLADVCNT,=AL2(1)                                             
         B     *+10                                                             
         MVC   PAR.PLTRNCNT,=AL2(1)                                             
         MVI   PAR.PLDATA,0                                                     
         MVC   PAR.PLRLEN,=Y(PLDATA+1-PLREC)                                    
*                                                                               
         LA    R3,PLDATA                                                        
         USING SRTELD,R3                                                        
BPAR04   CLI   SRTEL,0                                                          
         BE    BPAR10                                                           
         CLI   SRTEL,SRTSRTQ                                                    
         BE    *+12                                                             
         CLI   SRTEL,SRTTOTQ                                                    
         BNE   BPAR08                                                           
*                                                                               
         XR    R0,R0                                                            
         IC    R0,SRTNUM                                                        
         LA    R6,SRTLIST                                                       
BPAR06   GOTO1 BLDPAREL,BOPARM,TRNRECD,(R6),BOELEM                              
         LH    R1,PAR.PLRLEN       ADD ELEMENT TO END OF RECORD                 
         LA    RF,PAR.PLREC-1(R1)                                               
         XR    RE,RE                                                            
         IC    RE,BOELEM+1                                                      
         EX    RE,*+4                                                           
         MVC   0(0,RF),BOELEM                                                   
         AR    RF,RE                                                            
         MVI   0(RF),0                                                          
         AR    R1,RE                                                            
         STH   R1,PAR.PLRLEN                                                    
         LA    R6,1(R6)                                                         
         BCT   R0,BPAR06                                                        
*                                                                               
BPAR08   XR    RF,RF                                                            
         IC    RF,SRTLN                                                         
         BXH   R3,RF,BPAR04                                                     
*                                                                               
BPAR10   DS    0H                                                               
         GOTO1 PUTPAR,BOPARM,PAR.PLSTD                                          
         B     BPAR02                                                           
*                                                                               
BPAR20   DS    0H                                                               
         B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD/MERGE PARAGRAPH TSAR RECORD                          *         
*                                                                     *         
* NTRY: P1 = A(TSAR RECORD TO ADD OR MERGE)                           *         
***********************************************************************         
         SPACE 1                                                                
PUTPAR   NTR1  ,                                                                
         L     R4,0(R1)                                                         
NEW      USING PLSTD,R4                                                         
         L     R5,AIO5                                                          
MID      USING PLSTD,R5                                                         
*                                                                               
         XR    R2,R2                                                            
         BCTR  R2,0                R2 = LOW COUNT                               
         LH    R3,AFPARCNT         R3 = HIGH COUNT                              
*                                                                               
PPAR02   DS    0H                                                               
         LA    RE,1(R2)            TEST (HIGH-LOW) = 1                          
         CR    RE,R3                                                            
         BE    PPAR06                                                           
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,0(R2,R3)                                                      
         SRL   R6,1                R6 = MID POSITION                            
         LR    RE,R6                                                            
         SLL   RE,1                                                             
         LA    RE,AFPARLST(RE)                                                  
         MVC   MID.PLNUM,0(RE)     GET MID RECORD                               
         GOTO1 ATSARIO,BOPARM,('TSAGET',MID.PLSTD)                              
*                                                                               
         GOTO1 CLCPAR,BOPARM,NEW.PLSTD,MID.PLSTD                                
         BE    PPAR04                                                           
         BL    *+10                                                             
         LR    R2,R6               LOW = MID                                    
         B     PPAR02                                                           
         LR    R3,R6               HIGH = MID                                   
         B     PPAR02                                                           
*                                                                               
PPAR04   DS    0H                                                               
         GOTO1 MRGPAR,BOPARM,MID.PLSTD,NEW.PLSTD                                
         B     PUTPARX                                                          
*                                                                               
PPAR06   DS    0H                                                               
         GOTO1 ADDPAR,BOPARM,NEW.PLSTD,(R3)                                     
*                                                                               
PUTPARX  DS    0H                                                               
         B     EXITY                                                            
         DROP  NEW,MID                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO COMPARE DATA OF PARAGRAPH TSAR RECORDS                   *         
*                                                                     *         
* NTRY: P1 = A(TSAR RECORD 1)                                         *         
*       P2 = A(TSAR RECORD 2)                                         *         
* EXIT: CC = EQUAL IF THE SAME                                        *         
*       CC = LOW IF RECORD 1 < RECORD 2                               *         
*       CC = HIGH IF RECORD 1 > RECORD 2                              *         
***********************************************************************         
         SPACE 1                                                                
CLCPAR   NTR1  ,                                                                
         LM    R4,R5,0(R1)                                                      
REC1     USING PLSTD,R4                                                         
REC2     USING PLSTD,R5            EACH SECTION'S PARS GROUPED TOGETHER         
         CLC   REC1.PLKLVL,REC2.PLKLVL                                          
         BNE   EXIT                                                             
         CLC   REC1.PLKSECT,REC2.PLKSECT                                        
         BNE   EXIT                                                             
*                                                                               
         LA    R2,REC1.PLDATA      COMPARE FIELD DATA ON RECORDS                
EL1      USING PARELD,R2                                                        
         LA    R3,REC2.PLDATA                                                   
EL2      USING PARELD,R3                                                        
         XR    RF,RF                                                            
CPAR02   DS    0H                                                               
         CLC   EL1.PAREL,EL2.PAREL                                              
         BNE   EXIT                                                             
         CLI   EL1.PAREL,PARSRTQ                                                
         BNE   CPAR10                                                           
         XR    RE,RE               COMPARE DATA FOR SHORTEST LENGTH             
         IC    RE,EL1.PARDLEN                                                   
         CLM   RE,1,EL2.PARDLEN                                                 
         BNH   *+8                                                              
         IC    RE,EL2.PARDLEN                                                   
         AHI   RE,-1                                                            
         BM    CPAR04                                                           
         EX    RE,CPARCLC                                                       
         BNE   EXIT                                                             
CPAR04   CLC   EL1.PARDLEN,EL2.PARDLEN DATA SAME - LONGER IS HIGHER             
         BNE   EXIT                                                             
         IC    RF,EL1.PARLN                                                     
         AR    R2,RF                                                            
         IC    RF,EL2.PARLN                                                     
         BXH   R3,RF,CPAR02                                                     
CPARCLC  CLC   EL1.PARDATA(0),EL2.PARDATA                                       
         DROP  EL1,EL2                                                          
*                                                                               
CPAR10   DS    0H                  SORT OUT SUB-HEADING FROM SUB-TOTAL          
         CLC   REC1.PLKSWHR,REC2.PLKSWHR                                        
         BNE   EXIT                                                             
         CLC   REC1.PLKSSUB,REC2.PLKSSUB                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CLCPARY  B     EXITY                                                            
         SPACE 1                                                                
         DROP  REC1,REC2                                                        
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO MERGE PARAGRAPH TSAR RECORD                              *         
*                                                                     *         
* NTRY: P1 = A(TSAR RECORD TO BE ADDED TO)                            *         
*       P2 = A(TSAR RECORD TO BE ADDED FROM)                          *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
MRGPAR   NTR1  ,                                                                
         LM    R4,R5,0(R1)                                                      
TO       USING PLSTD,R4                                                         
FROM     USING PLSTD,R5                                                         
*                                                                               
         ICM   RE,3,TO.PLTRNCNT    UPDATE TRANSACTION COUNT                     
         ICM   RF,3,FROM.PLTRNCNT                                               
         AR    RE,RF                                                            
         STCM  RE,3,TO.PLTRNCNT                                                 
         CLI   TO.PL1STADV,0       TEST/SET FIRST ADVANCE                       
         BNE   *+10                                                             
         MVC   TO.PL1STADV,FROM.PL1STADV                                        
         ICM   RE,3,TO.PLADVCNT    UPDATE ADVANCE COUNT                         
         ICM   RF,3,FROM.PLADVCNT                                               
         AR    RE,RF                                                            
         STCM  RE,3,TO.PLADVCNT                                                 
*                                                                               
         XR    RF,RF                                                            
         LA    R2,TO.PLDATA                                                     
TOEL     USING PARELD,R2                                                        
         LA    R3,FROM.PLDATA                                                   
FROMEL   USING PARELD,R3                                                        
         XR    RF,RF                                                            
*                                                                               
MPAR02   CLI   TOEL.PAREL,0                                                     
         BE    MPAR10                                                           
*                                                                               
         CLI   TOEL.PAREL,PARTOTQ  UPDATE TOTAL                                 
         BNE   MPAR08                                                           
         AP    TOEL.PARAMT,FROMEL.PARAMT                                        
*                                                                               
MPAR08   IC    RF,TOEL.PARLN                                                    
         AR    R3,RF                                                            
         BXH   R2,RF,MPAR02                                                     
*                                                                               
MPAR10   DS    0H                                                               
         GOTO1 ATSARIO,BOPARM,('TSAPUT',TO.PLSTD)                               
         B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD PARAGRAPH TSAR RECORD                                *         
*                                                                     *         
* NTRY: P1 = A(TSAR RECORD TO BE ADDED TO)                            *         
*       P2 = POSITION IN LIST TSAR RECORD SHOULD GO BEFORE            *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
ADDPAR   NTR1  ,                                                                
         LM    R4,R5,0(R1)                                                      
         LH    RE,AFPARCNT                                                      
         LA    RE,1(RE)                                                         
         CHI   RE,AFPARMAX                                                      
         BL    *+6                                                              
         DC    H'0'                                                             
         STH   RE,AFPARCNT         UPDATE PARAGRAPH COUNT                       
TO       USING PLSTD,R4                                                         
*                                                                               
         MVC   TO.PLKNUM,AFPARCNT  TSAR RECORD ALWAYS ADDED AT END              
         GOTO1 ATSARIO,BOPARM,('TSAADD',TO.PLSTD)                               
*                                                                               
         LH    R0,AFPARCNT                                                      
         SR    R0,R5               R0 = NUMBER OF ITEMS TO MOVE                 
         BP    *+6                                                              
         DC    H'0'                                                             
         SLL   R5,1                                                             
         LA    R5,AFPARLST(R5)     R5 = A(INSERT POSITION)                      
         MVC   BOHALF1,TO.PLNUM                                                 
APAR06   MVC   BOHALF2,BOHALF1     SHUFFLE EVERYTHING UP                        
         MVC   BOHALF1,0(R5)                                                    
         MVC   0(L'PLNUM,R5),BOHALF2                                            
         LA    R5,L'PLNUM(R5)                                                   
         BCT   R0,APAR06                                                        
*                                                                               
         B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD FILE RECORDS                                         *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BLDFIL   NTR1  ,                                                                
         XC    AFBFMDA,AFBFMDA                                                  
         L     R5,ALSVALS                                                       
         USING PLSTD,R5                                                         
         L     R6,AIO2                                                          
O        USING PLSTD,R6                                                         
         XC    O.PLKEY,O.PLKEY                                                  
         LH    R0,AFPARCNT                                                      
         LTR   R0,R0                                                            
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$YHDAY)                                           
         B     EXITN                                                            
*                                                                               
         L     RF,ALINK            GET BILL HEADER RECORD IN IO4                
         MVC   IODAOVER,BEWHDRDA-LINKD(RF)                                      
         LHI   R1,IOGETRUP+IOACCMST+IO4                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,AFPARLST                                                      
BFIL02   MVC   PLNUM,0(R4)                                                      
         GOTO1 ATSARIO,TSAGET                                                   
*                                                                               
         CLC   PLKLVL,O.PLKLVL     TEST CHANGE OF LEVEL                         
         BE    BFIL04                                                           
         XC    O.PLKSECT,O.PLKSECT                                              
         XC    AFSECSEQ,AFSECSEQ   CLEAR SECTION SEQUENCE #                     
*                                                                               
BFIL04   DS    0H                                                               
         CLC   PLKSECT,O.PLKSECT   TEST CHANGE OF SECTION                       
         BE    BFIL06                                                           
         GOTO1 SECHDR,BOPARM,PLSTD ADD SECTION HEADER                           
         XC    AFPARSEQ,AFPARSEQ   CLEAR PARAGRAPH SEQUENCE #                   
*                                                                               
BFIL06   DS    0H                  ADD SECTION PARAGRAPH RECORD                 
         GOTO1 SECPAR,BOPARM,PLSTD,O.PLSTD                                      
*                                                                               
         LA    RE,PLREC            COPY OLD RECORD                              
         LH    RF,PLRLEN                                                        
         LA    R2,O.PLREC                                                       
         LR    R3,RF                                                            
         MVCL  R2,RE                                                            
*                                                                               
         LA    R4,L'AFPARLST(R4)                                                
         BCT   R0,BFIL02                                                        
*                                                                               
         L     R3,AIO4                                                          
         LA    R3,BEDRFST-BEDRECD(R3)                                           
         USING SCIELD,R3                                                        
         XR    RF,RF                                                            
BFIL12   CLI   SCIEL,SCIELQ                                                     
         BNE   *+12                                                             
         CLI   SCITYPE,SCITCBAP                                                 
         BE    BFIL14                                                           
         CLI   SCIEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    RF,SCILN                                                         
         BXH   R3,RF,BFIL12                                                     
BFIL14   ZAP   SCIAMNT,AFTOTNET    SET TOTAL NET/COMMISSION                     
         ZAP   SCIADMN,AFTOTCOM                                                 
         LHI   R1,IOPUT+IOACCMST+IO4                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDFILX  B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD SECTION HEADER                                       *         
*                                                                     *         
* NTRY: P1 = A(PARAGRAPH TSAR RECORD)                                 *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
SECHDR   NTR1  ,                                                                
         L     R5,0(R1)                                                         
P        USING PLSTD,R5                                                         
*                                                                               
         L     R4,AIO3                                                          
         USING FLSTD,R4            GET TSAR RECORD FOR SECTION TOTALS           
         XC    FLKEY,FLKEY                                                      
         MVI   FLKTYP,FLKTYPQ                                                   
         MVC   FLKLVL,P.PLKLVL                                                  
         MVC   FLKSECT,P.PLKSECT                                                
         GOTO1 ATSARIO,BOPARM,('TSARDH',FLSTD)                                  
         BE    SHDR02                                                           
         XC    FLKEY,FLKEY         MAY BE CATCH ALL SECTION                     
         MVI   FLKTYP,FLKTYPQ                                                   
         MVC   FLKLVL,P.PLKLVL                                                  
         MVI   FLKCALL,FLKCALLQ                                                 
         MVC   FLKSECT,P.PLKSECT                                                
         GOTO1 ATSARIO,BOPARM,('TSARDH',FLSTD)                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SHDR02   DS    0H                                                               
         L     R2,AIO1                                                          
         USING BEDRECD,R2                                                       
*                                  INITIALIZE RECORD                            
         XC    BEDRECD(BEDRFST+1-BEDRECD),BEDRECD                               
         L     RF,ALINK                                                         
         MVC   BEDKEY,BEWHDRKY-LINKD(RF)                                        
         MVC   BEDKLVL,FLKLVL                                                   
         MVC   BEDKWHER,=AL2(BEDKWBDQ)                                          
         IC    RF,AFSECSEQ         SET SECTION SEQ#                             
         LA    RF,1(RF)                                                         
         STC   RF,AFSECSEQ                                                      
         STC   RF,BEDKSSEQ                                                      
         MVC   BEDRSECT,P.PLKSECT                                               
         MVC   BEDRLEN,=AL2(BEDRFST+1-BEDRECD)                                  
*                                                                               
         USING PGHELD,BOELEM       ADD MISNAMED PARAGRAPH HEADER EL             
         XC    PGHELD(PGHLNQ),PGHELD                                            
         MVI   PGHEL,PGHELQ                                                     
         MVI   PGHLN,PGHLNQ                                                     
         ZAP   PGHNET,FLNET                                                     
         ZAP   PGHCOM,FLCOM                                                     
         MVC   PGHHTYP,BEDRSECT                                                 
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),BEDRECD,PGHELD,0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AIO,IOADD+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SECHDRX  B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD PARAGRAPH                                            *         
*                                                                     *         
* NTRY: P1 = A(PARAGRAPH TSAR RECORD)                                 *         
*       P2 = A(PREVIOUS TSAR RECORD)                                  *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
SECPAR   NTR1  ,                                                                
         LM    R5,R6,0(R1)                                                      
         USING PLSTD,R5                                                         
O        USING PLSTD,R6                                                         
         L     R7,AIO5                                                          
         USING BFMRECD,R7                                                       
         CLC   AFBFMDA,PLKBFMDA    TEST ALREADY HAVE FORMAT RECORD              
         BE    SPAR10                                                           
         MVC   AFBFMDA,PLKBFMDA                                                 
         MVC   IODAOVER,PLKBFMDA                                                
         LHI   R1,IOGET+IOACCMST+IO5                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    BFMRSTAT,BFMSCONT                                                
         BZ    *+6                                                              
         DC    H'0'                ?? (BEGINNING TO WONDER IF WORTH IT)         
*                                                                               
SPAR10   DS    0H                                                               
K        USING BEDRECD,AFBEDKEY    INITIALIZE KEY                               
         L     RF,ALINK                                                         
         MVC   K.BEDKEY,BEWHDRKY-LINKD(RF)                                      
         MVC   K.BEDKLVL,PLKLVL                                                 
         MVC   K.BEDKWHER,=AL2(BEDKWBDQ)                                        
         MVC   K.BEDKSSEQ,AFSECSEQ                                              
         LH    RE,AFPARSEQ         SET PARAGRAPH SEQUENCE #                     
         LA    RE,1(RE)                                                         
         STH   RE,AFPARSEQ                                                      
         STCM  RE,3,K.BEDKPARA                                                  
S        USING BEDKSTA,AFBEDSTA                                                 
         XC    S.BEDKSTA,S.BEDKSTA                                              
         MVC   S.BEDKSECT,PLKSECT                                               
         MVC   S.BEDKSLVL,PLKSWHR                                               
         MVC   S.BEDKSSUB,PLKSSUB                                               
         DROP  S,K                                                              
         NI    AFINDS,FF-AFIPAREL                                               
*                                                                               
SPAR20   DS    0H                  ADD AMOUNT BUCKETS TO RECORD                 
         USING FWTELD,BOELEM                                                    
         XC    FWTELD(FWTLNQ),FWTELD                                            
         MVI   FWTEL,FWTELQ                                                     
         MVI   FWTLN,FWTLNQ                                                     
*                                                                               
         NI    AFINDS,FF-(AFIHRS+AFIRATE)                                       
         LA    R3,PLDATA                                                        
         USING PARELD,R3                                                        
SPAR22   CLI   PAREL,0                                                          
         BE    SPAR30                                                           
         CLI   PAREL,PARSRTQ                                                    
         BE    *+12                                                             
         CLI   PAREL,PARTOTQ                                                    
         BNE   SPAR28                                                           
         GOTO1 GETFLD,BOPARM,('R4',PARFLD)                                      
         USING FLDTABD,R4                                                       
         TM    FLDINDS,FLDIAMT                                                  
         BZ    SPAR28                                                           
         MVC   FWTFLD,PARFLD                                                    
         ZAP   FWTAMT,PARAMT                                                    
         GOTO1 SECEL,BOPARM,FWTELD                                              
         CLI   PARFLD,BLFFNETQ     SAVE NET IN BODUB2                           
         BNE   *+10                                                             
         ZAP   BODUB2,PARAMT                                                    
         CLI   PARFLD,BLFFHRSQ     SAVE HOURS IN BODUB3                         
         BNE   *+14                                                             
         ZAP   BODUB3,PARAMT                                                    
         OI    AFINDS,AFIHRS                                                    
         CLI   PARFLD,BLFFRATQ     OR SAVE RATE IN BODUB3                       
         BNE   *+14                                                             
         ZAP   BODUB3,PARAMT                                                    
         OI    AFINDS,AFIRATE                                                   
         DROP  R4                                                               
*                                                                               
SPAR28   XR    RF,RF                                                            
         IC    RF,PARLN                                                         
         BXH   R3,RF,SPAR22                                                     
         DROP  R3                                                               
*                                                                               
SPAR30   DS    0H                  TEST HAVE JUST ONE OF HOURS/RATE             
         TM    AFINDS,AFIHRS+AFIRATE                                            
         BNM   SPAR40                                                           
         MVI   FWTFLD,BLFFRATQ                                                  
         TM    AFINDS,AFIRATE                                                   
         BZ    *+8                                                              
         MVI   FWTFLD,BLFFHRSQ                                                  
*                                                                               
         XC    BODUB1,BODUB1       BODUB1/BODUB2 = AMOUNT                       
         SRP   BODUB1(16),4,0                                                   
         CP    BODUB3,BCPZERO      ??                                           
         BNE   *+14                                                             
         ZAP   BODUB1,BCPZERO                                                   
         B     SPAR32                                                           
         DP    BODUB1(16),BODUB3   DIVIDE BY HOURS OR RATE                      
         SRP   BODUB1,64-2,5                                                    
SPAR32   ZAP   FWTAMT,BODUB1       = RATE OR HOURS                              
         GOTO1 SECEL,BOPARM,FWTELD                                              
*                                                                               
SPAR40   DS    0H                                                               
         LA    R3,BFMRFST                                                       
         USING BFPELD,R3                                                        
SPAR42   CLI   BFPEL,0                                                          
         BE    SECPARX                                                          
         CLI   BFPEL,BFPELQ        TEST FOR PARA ELEMENT                        
         BNE   SPAR44                                                           
         CLC   PLKSWHR,=AL2(BEDKSPRQ)                                           
         BNE   SPAR46                                                           
         OC    PLTRNCNT,PLTRNCNT   TEST PARA BUILT FROM 1 ADVANCE               
         BNZ   SPAR46                                                           
         CLC   PLADVCNT,=AL2(1)                                                 
         BNE   SPAR46                                                           
         MVC   BFPADV,PL1STADV     SAVE WHICH ADVANCE                           
         B     SPAR46                                                           
         DROP  R3                                                               
*                                                                               
         USING BFMELD,R3                                                        
SPAR44   DS    0H                                                               
         CLI   BFMEL,BFXELQ                                                     
         BE    SPAR46                                                           
         CLI   BFMEL,BFMELQ                                                     
         BNE   SPAR48                                                           
         CLI   BFMTYPE,BFMTDATA                                                 
         BNE   SPAR46                                                           
         CLI   BFMBODY,0           TEST FOR PANEL FIELD                         
         BNE   *+12                                                             
         OI    AFBEDSTA+(BEDKSTAT-BEDKSTA),BEDSPAN                              
         B     SPAR46                                                           
         GOTO1 TSTREQ,BOPARM,BFMELD,PLSTD,O.PLSTD                               
         BNE   SPAR48                                                           
         GOTO1 SECCNV,BOPARM,PLSTD,BFMELD                                       
         B     SPAR48                                                           
*                                                                               
SPAR46   DS    0H                                                               
         GOTO1 SECEL,BOPARM,BFMELD COPY ELEMENT TO BEDRECD                      
*                                                                               
SPAR48   XR    RF,RF                                                            
         IC    RF,BFMLN                                                         
         BXH   R3,RF,SPAR42                                                     
         DROP  R3                                                               
*                                                                               
SECPARX  DS    0H                                                               
         GOTO1 SECADD                                                           
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CONVERT BFMELD FOR BEDRECD                               *         
*                                                                     *         
* NTRY: P1 = A(PARAGRAPH TSAR RECORD)                                 *         
*       P2 = A(BFMELD)                                                *         
* EXIT: BFMELD ADDED TO BEDRECD                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
SECCNV   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING PLSTD,R2                                                         
         USING BFMELD,R3                                                        
*                                                                               
         LA    R4,PLDATA                                                        
         USING PARELD,R4                                                        
         XR    RF,RF                                                            
SCNV02   CLI   PAREL,0             FIND PARELD FOR FIELD                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PAREL,PARSRTQ                                                    
         BE    *+12                                                             
         CLI   PAREL,PARTOTQ                                                    
         BNE   SCNV08                                                           
         CLC   PARFLD,BFMBODY                                                   
         BE    SCNV10                                                           
SCNV08   IC    RF,PARLN                                                         
         BXH   R4,RF,SCNV02                                                     
*                                                                               
SCNV10   DS    0H                  CONVERT DATA TO TEXT                         
         GOTO1 CNVPAREL,BOPARM,PARELD,BFMELD                                    
N        USING BFMELD,BOELEM       COPY BFMELD AND ADD TEXT                     
         XR    RF,RF                                                            
         IC    RF,BFMLN                                                         
         EX    RF,*+4                                                           
         MVC   N.BFMELD(0),BFMELD                                               
         XR    R5,R5                                                            
         ICM   R5,1,AFCNVLEN                                                    
         BZ    SECCNVX                                                          
         STCM  R5,3,N.BFMTLEN                                                   
         CHI   R5,BFMTMAXL                                                      
         BNH   *+8                                                              
         LA    R5,BFMTMAXL                                                      
         EX    R5,*+4                                                           
         MVC   N.BFMTEXT(0),AFCNVTXT                                            
         AR    RF,R5                                                            
         STC   RF,N.BFMLN                                                       
         GOTO1 SECEL,BOPARM,N.BFMELD                                            
         DROP  N                                                                
*                                                                               
         CLM   R5,1,AFCNVLEN       TEST BFMELD NOT BIG ENOUGH FOR TEXT          
         BNL   SECCNVX                                                          
         IC    R5,AFCNVLEN                                                      
         AHI   R5,-BFMTMAXL                                                     
N        USING BFXELD,BOELEM                                                    
         MVI   N.BFXEL,BFXELQ                                                   
         LA    RE,BFXLNQ(R5)                                                    
         STC   RE,N.BFXLN                                                       
         EX    R5,*+4                                                           
         MVC   N.BFXTEXT(0),AFCNVTXT+BFMTMAXL                                   
         GOTO1 SECEL,BOPARM,N.BFXELD                                            
         DROP  N                                                                
*                                                                               
SECCNVX  DS    0H                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST IF FIELD IS REQUIRED FOR PARAGRAPH                  *         
*                                                                     *         
* NTRY1: P1 = A(BFMELD)                                               *         
*        P2 = A(CURRENT TSAR RECORD)                                  *         
*        P3 = A(PREVIOUS TSAR RECORD)                                 *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
TSTREQ   NTR1  ,                                                                
         LM    R3,R5,0(R1)                                                      
         USING BFMELD,R3                                                        
NREC     USING PLSTD,R4                                                         
OREC     USING PLSTD,R5                                                         
*                                                                               
         CLI   BFMDPER,BFMDPTRN    REQUIRED IF DISPLAY PER TRANSACTION          
         BE    EXITY                                                            
*                                  TEST FOR NORMAL PARAGRPAH                    
         CLC   NREC.PLKSWHR,=AL2(BEDKSPRQ)                                      
         BNE   EXITY               NO - MUST BE (SUB)-TOTAL                     
*                                  TEST FOR SAME LEVEL / SECTION                
         CLC   NREC.PLKLVL,OREC.PLKLVL                                          
         BNE   EXITY                                                            
         CLC   NREC.PLKSECT,OREC.PLKSECT                                        
         BNE   EXITY                                                            
*                                                                               
         CLI   BFMDPER,0                                                        
         BNE   *+10                                                             
         MVC   BFMDPER,BFMBODY     DEFAULT IS PER ITSELF                        
*                                                                               
         LA    R6,NREC.PLDATA                                                   
N        USING PARELD,R6                                                        
         LA    R7,OREC.PLDATA                                                   
O        USING PARELD,R7                                                        
         XR    RF,RF                                                            
TREQ02   CLI   N.PAREL,0                                                        
         BE    EXITY                                                            
         CLI   N.PAREL,PARTOTQ     TOTALS NEED TO BE DISPLAYED                  
         BNE   TREQ04                                                           
         CLC   N.PARFLD,BFMDPER                                                 
         BE    EXITY                                                            
         B     TREQ08                                                           
*                                                                               
TREQ04   CLI   N.PAREL,PARSRTQ                                                  
         BNE   TREQ08                                                           
         IC    RF,N.PARLN                                                       
         BCTR  RF,0                                                             
         CLC   N.PARELD(0),O.PARELD                                             
         EX    RF,*-6                                                           
         BNE   EXITY               REQUIRED IF CHANGE OF DATA                   
*                                                                               
         CLC   N.PARFLD,BFMDPER    IF SAME UP TO THIS DATA - NOT REQ.           
         BE    EXITN                                                            
*                                                                               
TREQ08   IC    RF,N.PARLN                                                       
         AR    R6,RF                                                            
         IC    RF,O.PARLN                                                       
         AR    R7,RF                                                            
         B     TREQ02                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD ELEMENT TO BEDRECD                                   *         
*                                                                     *         
* NTRY: P1 = A(ELEMENT TO ADD)                                        *         
***********************************************************************         
         SPACE 1                                                                
SECEL    NTR1  ,                                                                
         L     R3,0(R1)                                                         
         L     R2,AIO1                                                          
         USING BEDRECD,R2                                                       
SEL00    DS    0H                                                               
         TM    AFINDS,AFIPAREL     TEST ADDED ELEMNT TO RECORD                  
         BO    SEL02                                                            
         XC    BEDRECD(BEDRFST+1-BEDRECD),BEDRECD                               
         MVC   BEDKEY,AFBEDKEY                                                  
         MVC   BEDRSTA,AFBEDSTA                                                 
         MVC   BEDRLEN,=AL2(BEDRFST+1-BEDRECD)                                  
         OI    AFINDS,AFIPAREL                                                  
*                                                                               
SEL02    LH    RF,BEDRLEN          TEST RECORD WILL BE TOO BIG                  
         XR    RE,RE                                                            
         IC    RE,1(R3)                                                         
         AR    RF,RE                                                            
         CHI   RF,2000                                                          
         BNL   SEL04                                                            
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),BEDRECD,(R3),ADDEND                  
         CLI   12(R1),0                                                         
         BE    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
SEL04    DS    0H                                                               
         DC    H'0'                MAKES LIFE TOO COMPLICATED                   
         OI    BEDRSTAT,BEDSCONT   SET RECORD CONTINUED                         
         GOTO1 SECADD              ADD RECORD                                   
KEY      USING BEDKEY,AFBEDKEY                                                  
         IC    RE,KEY.BEDKSEQ      INCREMENT SEQUENCE NUMBER                    
         LA    RE,1(RE)                                                         
         STC   RE,KEY.BEDKSEQ                                                   
         CLI   BEDKSEQ,0                                                        
         BNE   SEL00                                                            
         DC    H'0'                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO ADD SECTION PARAGRAPH RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
SECADD   NTR1  ,                                                                
         TM    AFINDS,AFIPAREL     TEST ANY ELEMENTS ON RECORD                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         USING BEDRECD,R2                                                       
         MVC   BEDRSTA,AFBEDSTA    STATUS BYTE MAY HAVE BEEN UPDATED            
         TM    BEDRSTAT,BEDSPAN    UPDATE PANELS IF ANY                         
         BZ    SADD02                                                           
         GOTO1 AUPDPAN,BOPARM,=C'ADD',BEDRECD,BEDRECD,AIO4                      
SADD02   GOTO1 AFMTTXT,BOPARM,(C'S',BEDRECD)                                    
         GOTO1 AIO,IOADD+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    AFINDS,FF-AFIPAREL                                               
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD PAREL                                              *         
*                                                                     *         
* NTRY: P1 = A(TRANSACTION RECORD)                                    *         
*       P2 = A(BLFLD VALUE)                                           *         
*       P3 = A(AREA FOR ELEMENT)                                      *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BLDPAREL NTR1  ,                                                                
         LM    R2,R4,0(R1)                                                      
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
         USING PARELD,R4                                                        
*                                                                               
         GOTO1 GETFLD,BOPARM,('R3',0(R3))                                       
         USING FLDTABD,R3                                                       
*                                                                               
         XC    PARELD(256),PARELD                                               
         MVI   PAREL,PARSRTQ                                                    
         TM    FLDINDS,FLDITOT                                                  
         BZ    *+8                                                              
         MVI   PAREL,PARTOTQ                                                    
         MVC   PARFLD,FLDCODE                                                   
         MVC   PARDLEN,FLDGLEN                                                  
*                                                                               
         BAS   RE,GETDATA                                                       
*                                                                               
BPAREL10 DS    0H                                                               
         IC    RE,PARDLEN                                                       
         LA    RE,PARLNQ(RE)                                                    
         STC   RE,PARLN                                                         
*                                                                               
BLDPAREX B     EXIT                                                             
         SPACE 1                                                                
GETDATA  NTR1  ,                                                                
         XR    RF,RF                                                            
         ICM   RF,3,FLDGET                                                      
         LA    RF,CLB63(RF)                                                     
         BR    RF                                                               
*                                                                               
GETWC    MVC   PARDATA(L'TRNKWORK),TRNKWORK                                     
         B     EXIT                                                             
*                                                                               
GETREF   MVC   PARDATA(L'TRNKREF),TRNKREF                                       
         B     EXIT                                                             
*                                                                               
GETDATE  MVC   PARDATA(L'TRNKDATE),TRNKDATE                                     
         B     EXIT                                                             
*                                                                               
GETSUPN  MVC   PARDLEN,AFCACLEN                                                 
         ICM   RE,1,AFCACLEN                                                    
         BZ    EXIT                                                             
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   PARDATA(0),AFCACNAM                                              
         B     EXIT                                                             
*                                                                               
GETNAR   XR    RE,RE                                                            
         IC    RE,TRNLN                                                         
         AHI   RE,-TRNLN1Q                                                      
         STC   RE,PARDLEN                                                       
         BNP   EXIT                                                             
         EX    RE,*+4                                                           
         MVC   PARDATA(0),TRNNARR                                               
         LA    RF,PARDATA-1(RE)    REMOVE TRAILING SPACES                       
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   RE,*-10                                                          
         STC   RE,PARDLEN                                                       
         B     EXIT                                                             
*                                                                               
GETNET   ZAP   PARAMT,AFTRNNET                                                  
         B     EXIT                                                             
*                                                                               
GETCOM   ZAP   PARAMT,AFTRNCOM                                                  
         B     EXIT                                                             
*                                                                               
GETGRS   ZAP   PARAMT,AFTRNGRS                                                  
         B     EXIT                                                             
*                                                                               
GETHRS   ZAP   PARAMT,AFTRNHRS                                                  
         B     EXIT                                                             
*                                                                               
GETRAT   ZAP   PARAMT,AFTRNRAT                                                  
         B     EXIT                                                             
*                                                                               
GETWCN   MVC   PARDLEN,AFWCNAML                                                 
         ICM   RE,1,AFWCNAML                                                    
         BZ    EXIT                                                             
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   PARDATA(0),AFWCNAME                                              
         B     EXIT                                                             
*                                                                               
GETCMR   ZAP   PARAMT,AFTRNCRT                                                  
         B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CONVERT PAREL                                            *         
*                                                                     *         
* NTRY: P1 = A(PAREL)                                                 *         
*       P2 = A(BFMELD ELEMENT)                                        *         
* EXIT: AFCNVLEN = LENGTH OF TEXT                                     *         
*       AFCNVTXT = OUTPUT TEXT                                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
CNVPAREL NTR1  ,                                                                
         LM    R3,R4,0(R1)                                                      
         USING PARELD,R3                                                        
         USING BFMELD,R4                                                        
         GOTO1 GETFLD,BOPARM,('R5',PARFLD)                                      
         USING FLDTABD,R5                                                       
*                                                                               
         MVI   AFCNVTXT,C' '                                                    
         MVC   AFCNVTXT+1(L'AFCNVTXT-1),AFCNVTXT                                
         XR    RF,RF                                                            
         ICM   RF,3,FLDCNV                                                      
         BNZ   CPAREL02                                                         
         IC    RE,PARLN            NO CONVERT ROUTINE - JUST COPY               
         AHI   RE,-PARLNQ                                                       
         STC   RE,AFCNVLEN                                                      
         BNP   CNVPAREX                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   AFCNVTXT(0),PARDATA                                              
         B     CPAREL04                                                         
*                                                                               
CPAREL02 DS    0H                                                               
         MVC   AFCNVLEN,FLDCLEN                                                 
         BAS   RE,CNVDATA                                                       
*                                                                               
CPAREL04 DS    0H                                                               
         TM    FLDINDS,FLDIRMTS    TEST REMOVE TRAILING SPACES                  
         BZ    CNVPAREX                                                         
         XR    RE,RE                                                            
         ICM   RE,1,AFCNVLEN                                                    
         BZ    CNVPAREX                                                         
         LA    RF,AFCNVTXT-1(RE)                                                
CPAREL06 CLI   0(RF),C' '                                                       
         BH    CPAREL08                                                         
         BCTR  RF,0                                                             
         BCT   RE,CPAREL06                                                      
CPAREL08 STC   RE,AFCNVLEN                                                      
*                                                                               
CNVPAREX DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
CNVDATA  NTR1  ,                                                                
         B     CLB63(RF)                                                        
*                                                                               
CNVDATE  DS    0H                                                               
         GOTO1 VDATCON,BOPARM,(X'41',PARDATA),(17,AFCNVTXT)                     
         XR    RE,RE                                                            
         IC    RE,AFCNVLEN                                                      
         LA    RF,AFCNVTXT-1(RE)                                                
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   RE,*-10                                                          
         STC   RE,AFCNVLEN                                                      
         B     EXIT                                                             
*                                                                               
CNVBAMT  DS    0H                  CONVERT BILLING AMOUNT                       
         CURED (P8,PARAMT),(30,AFCNVTXT),CSCURBIL,MINUS=YES,           *        
               DMCB=BODMCB,COMMAS=YES,ALIGN=LEFT                                
         STC   R0,AFCNVLEN                                                      
         B     EXITY                                                            
*                                                                               
CNVRATE  DS    0H                                                               
         CURED (P8,PARAMT),(30,AFCNVTXT),2,MINUS=YES,                  *        
               DMCB=BODMCB,COMMAS=YES,ALIGN=LEFT                                
         STC   R0,AFCNVLEN                                                      
         B     EXITY                                                            
*                                                                               
CNVHRS   DS    0H                                                               
         CURED (P8,PARAMT),(30,AFCNVTXT),2,MINUS=YES,                  *        
               DMCB=BODMCB,COMMAS=YES,ALIGN=LEFT                                
         STC   R0,AFCNVLEN                                                      
         B     EXITY                                                            
*                                                                               
CNVCMR   DS    0H                                                               
         CURED (P8,PARAMT),(30,AFCNVTXT),4,MINUS=YES,                  *        
               DMCB=BODMCB,COMMAS=YES,ALIGN=LEFT                                
         STC   R0,AFCNVLEN                                                      
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET FLDTABD ENTRY                                        *         
*                                                                     *         
* NTRY: P1 BYTE 0 = RX                                                *         
*             1-3 = A(BLFFLD) VALUE                                   *         
* EXIT:        RX = A(FLDTABD) ENTRY                                  *         
***********************************************************************         
         SPACE 1                                                                
GETFLD   STM   RE,R2,12(RD)                                                     
*                                                                               
         XR    RE,RE                                                            
         IC    RE,0(R1)            RE = REGISTER TO CONTAIN FLDTABD             
         XR    R2,R2                                                            
         ICM   R2,7,1(R1)                                                       
         LA    RF,FLDTAB                                                        
         USING FLDTABD,RF                                                       
GFLD02   CLC   FLDCODE,0(R2)                                                    
         BE    GFLD04                                                           
         LA    RF,FLDTABL(RF)                                                   
         CLI   FLDCODE,EOT                                                      
         BNE   GFLD02                                                           
         DC    H'0'                                                             
         DROP  RF                                                               
*                                                                               
GFLD04   LM    R0,R2,20(RD)                                                     
         SLL   RE,4                                                             
         EX    RE,GETFLDEX                                                      
         LM    RE,RF,12(RD)                                                     
         BR    RE                                                               
         SPACE 1                                                                
GETFLDEX LR    R0,RF                                                            
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
         EJECT                                                                  
***********************************************************************         
* FIELD TABLE                                                         *         
***********************************************************************         
         SPACE 1                                                                
FLDTABD  DSECT                                                                  
FLDCODE  DS    XL1                 BLFFLD VALUE                                 
FLDINDS  DS    XL1                 INDICATOR BYTE                               
FLDISORT EQU   X'80'               RECORD CAN BE SORTED BY THIS TYPE            
FLDIAMT  EQU   X'40'               FIELD IS AN AMOUNT (HELD ON FWTELD)          
FLDITOT  EQU   X'20'               FIELD IS TOTALLED AMOUNT                     
FLDIRMTS EQU   X'10'               REMOVE TRAILING SPACES                       
FLDGLEN  DS    XL1                 LENGTH OF DATA                               
FLDGET   DS    AL2                 DISP. TO EXTRACT DATA ROUTINE                
FLDCLEN  DS    XL1                 LENGTH OF CONVERTED DATA                     
FLDCNV   DS    AL2                 DISP. TO CONVERT ROUTINE / 0                 
FLDTABL  EQU   *-FLDTABD                                                        
         SPACE 1                                                                
CLB63    CSECT                                                                  
FLDTAB   DS    0XL(FLDTABL)                                                     
*                                                                               
         DC    AL1(BLFFWCQ,FLDISORT+FLDIRMTS)                                   
         DC    AL1(L'TRNKWORK),AL2(GETWC-CLB63)                                 
         DC    AL1(0),AL2(0)                                                    
*                                                                               
         DC    AL1(BLFFREFQ,FLDISORT+FLDIRMTS)                                  
         DC    AL1(L'TRNKREF),AL2(GETREF-CLB63)                                 
         DC    AL1(0),AL2(0)                                                    
*                                                                               
         DC    AL1(BLFFDATQ,FLDISORT+FLDIRMTS)                                  
         DC    AL1(L'TRNKDATE),AL2(GETDATE-CLB63)                               
         DC    AL1(8),AL2(CNVDATE-CLB63)                                        
*                                                                               
         DC    AL1(BLFFSUPQ,FLDISORT+FLDIRMTS)                                  
         DC    AL1(0),AL2(GETSUPN-CLB63)                                        
         DC    AL1(0),AL2(0)                                                    
*                                                                               
         DC    AL1(BLFFNARQ,FLDISORT+FLDIRMTS)                                  
         DC    AL1(0),AL2(GETNAR-CLB63)                                         
         DC    AL1(0),AL2(0)                                                    
*                                                                               
         DC    AL1(BLFFNETQ,FLDIAMT+FLDITOT)                                    
         DC    AL1(L'PARAMT),AL2(GETNET-CLB63)                                  
         DC    AL1(0),AL2(CNVBAMT-CLB63)                                        
*                                                                               
         DC    AL1(BLFFCOMQ,FLDIAMT+FLDITOT)                                    
         DC    AL1(L'PARAMT),AL2(GETCOM-CLB63)                                  
         DC    AL1(0),AL2(CNVBAMT-CLB63)                                        
*                                                                               
         DC    AL1(BLFFGRSQ,FLDIAMT+FLDITOT)                                    
         DC    AL1(L'PARAMT),AL2(GETGRS-CLB63)                                  
         DC    AL1(0),AL2(CNVBAMT-CLB63)                                        
*                                                                               
         DC    AL1(BLFFHRSQ,FLDIAMT+FLDITOT)                                    
         DC    AL1(L'PARAMT),AL2(GETHRS-CLB63)                                  
         DC    AL1(0),AL2(CNVHRS-CLB63)                                         
*                                                                               
         DC    AL1(BLFFRATQ,FLDISORT+FLDIAMT)                                   
         DC    AL1(L'PARAMT),AL2(GETRAT-CLB63)                                  
         DC    AL1(0),AL2(CNVRATE-CLB63)                                        
*                                                                               
         DC    AL1(BLFFWCNQ,FLDISORT+FLDIRMTS)                                  
         DC    AL1(0),AL2(GETWCN-CLB63)                                         
         DC    AL1(0),AL2(0)                                                    
*                                                                               
         DC    AL1(BLFFCMRQ,FLDISORT)                                           
         DC    AL1(L'PARAMT),AL2(GETCMR-CLB63)                                  
         DC    AL1(0),AL2(CNVCMR-CLB63)                                         
*                                                                               
FLDTABX  DS    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* SECTION FILTER TABLE                                                *         
***********************************************************************         
         SPACE 1                                                                
FLTTABD  DSECT                                                                  
FLTTYPE  DS    XL1                 BSDSTYP VALUE                                
FLTLEN   DS    XL1                 LENGTH OF DATA ENTRY                         
FLTROUT  DS    AL2                 DISP. TO TEST ROUTINE                        
FLTTABL  EQU   *-FLTTABD                                                        
         SPACE 1                                                                
CLB63    CSECT                                                                  
FLTTAB   DS    0XL(FLTTABL)                                                     
         DC    AL1(BSDSWCT,1),AL2(TSTWCT-CLB63)                                 
         DC    AL1(BSDSWCG,1),AL2(TSTWCG-CLB63)                                 
         DC    AL1(BSDSWC,2),AL2(TSTWC-CLB63)                                   
         DC    AL1(BSDSTRN,1),AL2(TSTTRN-CLB63)                                 
         DC    AL1(BSDSCAC,14),AL2(TSTCAC-CLB63)                                
*        DC    AL1(BSDSCAT,10),AL2(TSTCAT-CLB63)                                
FLTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
* ACCLBFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBFILE                                                      
         PRINT ON                                                               
* ACCLBLINK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBLINK                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* SECTION FILTER TSAR RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
FLSTD    DSECT                                                                  
FLST     DS    0XL768                                                           
FLNUM    DS    XL2                 TSAR LIST RECORD NUMBER                      
*                                                                               
FLREC    DS    0XL766              ** TSAR LIST RECORD **                       
FLRLEN   DS    XL2                 RECORD LENGTH                                
*                                                                               
FLKEY    DS    0XL32               * RECORD KEY *                               
FLKTYP   DS    XL1                 RECORD TYPE                                  
FLKTYPQ  EQU   X'FA'               SECTION PARAGRAPH                            
FLKLVL   DS    XL2                 LEVEL                                        
FLKCALL  DS    XL1                 CATCH ALL INDICATOR                          
FLKCALLQ EQU   X'FF'               CATCH ALL SECTION MUST COME LAST             
FLKSECT  DS    XL1                 SECTION NUMBER                               
         ORG   FLKEY+L'FLKEY                                                    
FLNET    DS    PL8                 SECTION NET TOTAL                            
FLCOM    DS    PL8                 SECTION COMMISSION TOTAL                     
FLDATA   DS    XL708               * RECORD DATA *                              
FLMAXLNQ EQU   *-FLREC             MAXIMUM LENGTH OF TSAR RECORD                
         SPACE 1                                                                
SFELD    DSECT                     * SECTION FILTER ELEMENT *                   
SFEL     DS    XL1                 ELEMENT CODE ( = BSDSTYP)                    
SFLN     DS    XL1                 ELEMENT LENGTH                               
SFNUM    DS    XL1                 NUMBER OF ENTRIES                            
SFLNQ    EQU   *-SFELD                                                          
SFDATA   DS    0C                  DATA ENTRIES                                 
         SPACE 1                                                                
***********************************************************************         
* PARAGRAPH TSAR RECORD                                               *         
***********************************************************************         
         SPACE 1                                                                
PLSTD    DSECT                                                                  
PLST     DS    0XL768                                                           
PLNUM    DS    XL2                 TSAR LIST RECORD NUMBER                      
*                                                                               
PLREC    DS    0XL766              ** TSAR LIST RECORD **                       
PLRLEN   DS    XL2                 RECORD LENGTH                                
*                                                                               
PLKEY    DS    0XL32               * RECORD KEY *                               
PLKTYP   DS    XL1                 RECORD TYPE                                  
PLKSRTQ  EQU   X'FB'               SORT RECORD                                  
PLKPARQ  EQU   X'FC'               SECTION PARAGRAPH                            
PLKNUM   DS    XL2                 RECORD NUMBER (PARAGRAPH ONLY)               
PLKLVL   DS    XL2                 LEVEL                                        
PLKSECT  DS    XL1                 SECTION NUMBER                               
PLKSWHR  DS    XL2                 WHERE WITHIN SECTION                         
PLKSSUB  DS    XL1                 SUB-SECTION LEVEL                            
PLKBFMDA DS    AL4                 DISK-ADDRESS OF BFMRECD                      
         ORG   PLKEY+L'PLKEY                                                    
PLTRNCNT DS    XL2                 NO. OF TRANSACTIONS (NON-ADVANCES)           
PLADVCNT DS    XL2                 NO. OF ADVANCES IN PARA                      
PL1STADV DS    XL1                 FIRST ADVANCE SEQUENCE NUMBER                
PLDATA   DS    XL727               * RECORD DATA *                              
PLMAXLNQ EQU   *-PLREC             MAXIMUM LENGTH OF TSAR RECORD                
         SPACE 1                                                                
SRTELD   DSECT                     ** SORT RECORD ELEMENT **                    
SRTEL    DS    XL1                 ELEMENT CODE                                 
SRTSRTQ  EQU   X'01'               FIELD DATA LIST FOR SORTING                  
SRTTOTQ  EQU   X'02'               FIELD DATA AMOUNTS FOR TOTALLING             
SRTLN    DS    XL1                 ELEMENT LENGTH                               
SRTNUM   DS    XL1                 NUMBER OF ENTRIES                            
SRTLNQ   EQU   *-SRTELD                                                         
SRTLIST  DS    0X                  LIST OF BLFFLDS                              
         SPACE 1                                                                
PARELD   DSECT                     ** PARAGRAPH RECORD ELEMENT **               
PAREL    DS    XL1                 ELEMENT CODE                                 
PARSRTQ  EQU   SRTSRTQ             DATA FOR SORTING                             
PARTOTQ  EQU   SRTTOTQ             TOTALLED DATA AMOUNT                         
PARLN    DS    XL1                 ELEMENT LENGTH                               
PARFLD   DS    XL1                 BLFFLD VALUE                                 
PARDLEN  DS    XL1                 DATA LENGTH                                  
PARLNQ   EQU   *-PARELD                                                         
PARDATA  DS    0C                                                               
PARAMT   DS    0PL8                AMOUNT (IFF DATA IS AMOUNT)                  
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
AFWORKD  DSECT                                                                  
*                                                                               
AFINDS   DS    XL1                 INDICATOR BYTE                               
AFICONT  EQU   X'80'               BFMRECD CONTINUED ON NEXT RECORD             
AFIPAREL EQU   X'40'               ELEMENT ADDED TO PARAGRPAH RECORD            
AFIHRS   EQU   X'20'               HOURS FWTELD ADDED                           
AFIRATE  EQU   X'10'               RATE FWTELD ADDED                            
AFIBXSEQ EQU   X'08'               BROKEN TRANSACTION READ SEQUENCE             
AFBFMDA  DS    AL4                 SAVED DISK ADDRESS FOR BFMRECD               
*                                                                               
AFLVLCNT DS    H                   LEVEL COUNT                                  
AFLVLMAX EQU   3                                                                
AFLVLLST DS    (AFLVLMAX)XL2       LEVEL LIST                                   
*                                                                               
AFADVKEY DS    XL(L'BEDKEY)        SAVED ADVANCE KEY                            
AFADVSEQ DS    XL1                 SAVED ADVANCE SEQUENCE NUMBER OR 0           
*                                                                               
AFCAC    DS    CL14                CURRENT CONTRA-ACCOUNT                       
AFCACLEN DS    XL1                 CONTRA-ACCOUNT NAME LENGTH                   
AFCACNAM DS    CL36                CONTRA-ACCOUNT NAME                          
*                                                                               
AFWC     DS    CL2                 CURRENT WORK-CODE                            
AFWCTYPE DS    XL1                 WORK-CODE TYPE (BSDTIMEQ/BSDCOSTQ)           
AFWCGRP  DS    CL1                 WORK-CODE GROUP                              
AFWCNAML DS    XL1                 WORK-CODE NAME LENGTH                        
AFWCNAME DS    CL36                WORK-CODE NAME                               
*                                                                               
         DS    0D                  TRANSACTION BILLED AMOUNTS                   
AFTRNNET DS    PL8                                                              
AFTRNCOM DS    PL8                                                              
AFTRNGRS DS    PL8                                                              
AFTRNHRS DS    PL8                                                              
AFTRNRAT DS    PL8                                                              
AFTRNCRT DS    PL8                                                              
*                                                                               
AFTOTNET DS    PL8                 TOTAL BILL NET AMOUNT                        
AFTOTCOM DS    PL8                 TOTAL BILL COMMISSION AMOUNT                 
*                                                                               
AFCNVLEN DS    XL1                 CONVERTED DATA LENGTH                        
AFCNVTXT DS    XL256               CONVERTED TEXT                               
*                                                                               
AFBEDKEY DS    XL(L'BEDKEY)        CURRENT PARAGRAPH RECORD KEY                 
AFBEDSTA DS    XL(L'BEDKSTA)       CURRENT PARAGRAPH STATUS AREA                
*                                                                               
AFSECSEQ DS    XL1                 SECTION SEQUENCE NUMBER                      
AFPARSEQ DS    H                   PARAGRPAH SEQUENCE NUBER                     
*                                                                               
AFPARCNT DS    H                   PARAGRAPH COUNT                              
AFPARMAX EQU   1024                                                             
AFPARLST DS    (AFPARMAX)XL2       PARAGRAPH LIST                               
*                                                                               
         DS    (OVERWRKL-(*-AFWORKD))X                                          
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACCLB63   08/16/00'                                      
         END                                                                    
