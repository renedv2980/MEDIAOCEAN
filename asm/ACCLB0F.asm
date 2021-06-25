*          DATA SET ACCLB0F    AT LEVEL 040 AS OF 08/16/00                      
*PHASE T6210FA                                                                  
CLB0F    TITLE '- BILL PROGRAM - PREVBILL'                                      
CLB0F    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLBF**,RR=RE                                                 
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         LH    R8,=Y(BSDICT-TWAD)                                               
         LA    R8,TWAD(R8)                                                      
         USING BSDICT,R8                                                        
         USING TRNRECD,IOKEY                                                    
         ST    RE,BORELO                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRK,RC                                                       
         MVI   CODTAB,EOT                                                       
*                                                                               
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     LSTFRST             FIRST FOR THIS LIST                          
         B     SCRFRST             FIRST FOR THIS SCREEN                        
         B     EXITY               LAST FOR THIS SCREEN                         
         B     EXITY               FIRST FOR VALIDATE THIS LINE                 
         B     EXITN               VALIDATE COLUMN                              
         B     EXITY               LAST FOR VALIDATE THIS LINE                  
         B     DISCLM              DISPLAY COLUMN                               
         B     GETFRST             GET FIRST RECORD FOR LIST                    
         B     GETNEXT             GET NEXT RECORD                              
         B     EXITY               SET UP MY OWN HEADING                        
         B     VALSEL              VALIDATE LINE SELECTION                      
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* FIRST FOR SCREEN                                                    *         
***********************************************************************         
         SPACE 1                                                                
SCRFRST  MVC   BASJOBC,BCJOBCOD                                                 
         MVC   BASJOBN,BCJOBNAM                                                 
*                                                                               
         L     RE,=A(VALOPT)                                                    
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,BASOPTH                                                    
         MVC   AOVEROUT,ALSVALS                                                 
         MVC   BOWORK1(L'LSOVER),LSOVER                                         
         XC    LSOVER,LSOVER                                                    
         XC    BOWORK2,BOWORK2                                                  
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         OC    BOWORK2(L'GOCBDPRV),GOCBDPRV-GOBBLOCK(RF)                        
         BNZ   *+10                                                             
         MVC   BOWORK2(DEFCLML),DEFCLM                                          
         GOTO1 AVALOPT,BOPARM,OPTTAB,BOWORK2,0                                  
         BNE   EXITL                                                            
*                                                                               
         CLC   LSOVER,BOWORK1           TEST CHANGE IN OPTIONS                  
         BE    EXITY                                                            
         B     EXITH                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
LSTFRST  XC    TRNKEY,TRNKEY       INITIALIZE THE KEY                           
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,BCJOBCOD                                                 
         MVC   TRNKWORK,REVWC                                                   
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* GET FIRST/NEXT RECORD FOR LIST                                      *         
***********************************************************************         
         SPACE 1                                                                
GETFRST  LA    R1,IOHIGH+IOACCDIR                                               
         B     *+8                                                              
GETNEXT  LA    R1,IOSEQ+IOACCDIR                                                
         GOTO1 AIO                                                              
         BNE   EXITN                                                            
         CLC   TRNKEY(TRNKCULC-TRNKEY),IOKEYSAV                                 
         BNE   EXITN                                                            
         OC    TRNKDATE,TRNKDATE   ENSURE HAVE TRANSACTION RECORD               
         BZ    GETNEXT                                                          
         CLC   TRNKDATE,BCSPACES                                                
         BE    GETNEXT                                                          
*                                                                               
         MVC   IODAOVER,TRNKDA                                                  
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
*                                                                               
         MVC   TLPPBIL,TRNKREF     SAVE BILL NO.                                
         MVC   TLPPCOD,CSCPYCUR    DEFAULT TO COMPANY CURRENCY                  
         L     R3,AIO1                                                          
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         USING TRNELD,R3                                                        
*&&US                                                                           
         LH    RE,=Y(UC@CLIBG-TWAD)                                             
         LA    RE,TWAD(RE)                                                      
         CLC   TRNNARR(L'UC@CLIBG),0(RE)                                        
         BNE   GETNEXT                                                          
*&&                                                                             
*&&UK                                                                           
         LH    RE,=Y(LC@CLIBG-TWAD)                                             
         LA    RE,TWAD(RE)                                                      
         IC    RF,TRNLN                                                         
         SH    RF,=Y(TRNLN1Q+1)                                                 
         BM    GETNEXT                                                          
         EX    RF,*+8                                                           
         BNE   GETNEXT             BILL DID NOT ORIGINATE FROM CBIL             
         CLC   TRNNARR(0),0(RE)                                                 
*&&                                                                             
         USING AFCELD,R3                                                        
         XR    RF,RF                                                            
GNEXT02  CLI   AFCEL,0                                                          
         BE    GNEXT10                                                          
         CLI   AFCEL,AFCELQ                                                     
         BE    GNEXT06                                                          
         CLI   AFCEL,RATETAXQ                                                   
         BE    GNEXT08                                                          
GNEXT04  IC    RF,AFCLN                                                         
         BXH   R3,RF,GNEXT02                                                    
GNEXT06  MVC   TLPPCOD,AFCCURR                                                  
         MVC   TLPPEXC,AFCX                                                     
         DROP  R3                                                               
         B     GNEXT04                                                          
         USING RATELD,R3                                                        
GNEXT08  CLC   RATRATE,=X'8000'    THIS MEANS FULLY REVERSED & UPDATED          
         BE    GETNEXT                                                          
         B     GNEXT04                                                          
         DROP  R3                                                               
*                                                                               
GNEXT10  GOTO1 CURTOTAB,BOPARM,TLPPCOD,TLPPTAB                                  
         BE    GETNEXTX                                                         
         GOTO1 AIO,IOREAD+IOACCDIR  BLDCUR DOES IO                              
*                                                                               
GETNEXTX B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY COLUMN                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISCLM   L     R2,AIO1                                                          
         USING TRNRECD,R2          R2=A(TRANSACTION RECORD)                     
         USING TRNELD,TRNRFST                                                   
*                                                                               
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         B     DISBIL              BILL NUMBER                                  
         B     DISDAT              DATE                                         
         B     DISNET              NET                                          
         B     DISCOM              COMMISSION                                   
         B     DISCUR              CURRENCY                                     
         SPACE 1                                                                
***********************************************************************         
* DISPLAY BILL NUMBER                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISBIL   NI    TLINDS1,FF-TLIHIGH                                               
         LA    R3,TRNELD                                                        
         USING RATELD,R3           R3=A(RATE ELEMENT)                           
         XR    RF,RF                                                            
DISBIL02 CLI   RATEL,0                                                          
         BE    DISBIL10                                                         
         CLI   RATEL,RATETAXQ                                                   
         BE    *+12                                                             
         IC    RF,RATLN                                                         
         BXH   R3,RF,DISBIL02                                                   
         OC    RATRATE,RATRATE     NON-ZERO                                     
         BZ    *+8                   MEANS ITEMS ALLOCATED FOR REVERSAL         
         OI    TLINDS1,TLIHIGH         SHOWED BY HIGHLIGHTING LINE              
         DROP  R3                                                               
*                                                                               
DISBIL10 MVC   FVIFLD(L'TRNKREF),TRNKREF                                        
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY DATE                                                        *         
***********************************************************************         
         SPACE 1                                                                
DISDAT   GOTO1 VDATCON,BOPARM,(1,TRNKDATE),(17,FVIFLD)                          
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY NET                                                         *         
***********************************************************************         
         SPACE 1                                                                
DISNET   CURED (P6,TRNAMNT),(13,FVIFLD),TLPPTAB,MINUS=YES,DMCB=BODMCB,*0        
               ZERO=NOBLANK                                                     
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY COMMISSION                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISCOM   LA    R3,TRNRFST                                                       
         USING SCIELD,R3                                                        
         ZAP   BODUB1,BCPZERO                                                   
         XR    RF,RF                                                            
DISCOM02 CLI   SCIEL,0                                                          
         BE    DISCOM04                                                         
         CLI   SCIEL,SCIELQ                                                     
         BNE   *+12                                                             
         CLI   SCITYPE,SCITCOMM                                                 
         BE    *+12                                                             
         IC    RF,SCILN                                                         
         BXH   R3,RF,DISCOM02                                                   
         ZAP   BODUB1,SCIAMNT                                                   
*                                                                               
DISCOM04 CURED (P8,BODUB1),(13,FVIFLD),TLPPTAB,MINUS=YES,DMCB=BODMCB, *0        
               ZERO=NOBLANK                                                     
         B     EXIT                                                             
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* DISPLAY CURRENCY CODE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISCUR   MVC   FVIFLD(L'TLPPCOD),TLPPCOD                                        
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE LINE SELECTION (SET BILL NUMBER)                           *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   STC   R1,OVACT            SAVE SELECT ACTION                           
         MVC   IODAOVER,BCJOBDA    GET JOB RECORD                               
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1                                                          
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         LR    R1,R3                                                            
         USING FFTELD,R1                                                        
         XC    OBILAUTR,OBILAUTR                                                
         SR    RF,RF                                                            
         B     *+12                                                             
VALS02   IC    RF,FFTLN                                                         
         AR    R1,RF                                                            
         CLI   FFTEL,0                                                          
         BE    VALS04                                                           
         CLI   FFTEL,FFTELQ                                                     
         BNE   VALS02                                                           
         CLI   FFTTYPE,FFTTAUTR                                                 
         BNE   VALS02                                                           
         OC    OBILAUTR,OBILAUTR   TEST AUTOREV BILL ALREADY FOUND              
         BNZ   EXITN               SINGLE AUTOREV BILL ONLY                     
         MVC   OBILAUTR,FFTDATA                                                 
         B     VALS02                                                           
*                                                                               
         USING AFCELD,R3                                                        
VALS04   CLI   AFCEL,0                                                          
         BE    VALS06                                                           
         CLI   AFCEL,AFCELQ                                                     
         BE    *+12                                                             
         IC    RF,AFCLN                                                         
         BXH   R3,RF,VALS04                                                     
         OC    AFCCURR,AFCCURR                                                  
         BZ    VALS06                                                           
         CLC   TLPPCOD,AFCCURR     TEST MATCHING CURRENCY                       
         BNE   EXITN                                                            
         CLC   TLPPCOD,CSCPYCUR    IF AGENCY CURRENCY DONT TEST RATE            
         BE    VALS06                                                           
         CLC   TLPPEXC,AFCX        TEST MATCHING EXCHANGE RATE                  
         BNE   EXITN                                                            
         DROP  R3                                                               
*                                                                               
VALS06   MVC   IODAOVER,TLDA       READ BILL                                    
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1                                                          
         SR    R1,R1               NOW PERFORM SPECIFIC ACTION                  
         IC    R1,OVACT                                                         
         SLL   R1,2                                                             
         B     *(R1)                                                            
         B     BILREV              1 - REVERSE                                  
         B     AUTREV              2 - AUTO REVERSE                             
         B     AUTCAR              3 - CLEAR AUTO REVERSE                       
         EJECT                                                                  
***********************************************************************         
* REVERSE                                                             *         
***********************************************************************         
         SPACE 1                                                                
BILREV   L     R1,AIO3                                                          
         TM    TRNRSTA2-TRNRECD(R1),TRNSBILP                                    
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$CREVA)                                           
         B     EXITN               AUTO REVERSE/UPDATED REVERSE BILL            
         OC    OBILAUTR,OBILAUTR   TEST AUTOREV BILL FOUND                      
         BZ    BILREV02                                                         
         CLC   OBILAUTR,TLPPBIL    TEST THIS IS THE ONE                         
         BNE   BILREV02                                                         
         MVC   FVMSGNO,=AL2(AE$CREVA)                                           
         B     EXITN               AUTO REVERSE BILL                            
*                                                                               
BILREV02 MVC   CSBILNUM,TLPPBIL    SET SELECTED BILL NUMBER                     
         MVC   CSBILCUR,TLPPCOD    SET SELECTED BILL CURRENCY CODE              
         MVC   CSEXCVAL,TLPPEXC    SET SELECTED BILL EXCHANGE RATE              
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* AUTO REVERSE                                                        *         
***********************************************************************         
         SPACE 1                                                                
AUTREV   L     R1,AIO3                                                          
         TM    TRNRSTA2-TRNRECD(R1),TRNSBILP                                    
         BO    *+14                                                             
         OC    OBILAUTR,OBILAUTR                                                
         BZ    AUTREV02                                                         
         MVC   FVMSGNO,=AL2(AE$CARAR)                                           
         B     EXITN               AUTO REVERSE/UPDATED REVERSE BILL            
*                                                                               
AUTREV02 LA    RF,TRNRFST-TRNRECD(R1)                                           
         USING RATELD,RF                                                        
         SR    R0,R0               SKIP STRAIGHT PAST TRNEL                     
AUTREV04 IC    R0,RATLN                                                         
         AR    RF,R0                                                            
         CLI   RATEL,0                                                          
         BE    AUTREV06                                                         
         CLI   RATEL,RATETAXQ                                                   
         BNE   AUTREV04                                                         
         OC    RATRATE,RATRATE                                                  
         BZ    AUTREV06                                                         
         MVC   FVMSGNO,=AL2(AE$CARRP)                                           
         B     EXITN               SOME STANDARD REVERSING PENDING              
*                                                                               
AUTREV06 MVC   CSBILNUM,TLPPBIL    SET SELECTED BILL NUMBER                     
         MVC   CSBILCUR,TLPPCOD    SET SELECTED BILL CURRENCY CODE              
         MVC   CSEXCVAL,TLPPEXC    SET SELECTED BILL EXCHANGE RATE              
*                                                                               
         GOTO1 VCOLY,BODMCB,('O#BILARV',0),0,0                                  
         L     RF,BODMCB                                                        
         ICM   RF,8,OVACT          PASS SELECT TABLE NUMBER                     
         BASR  RE,RF                                                            
         BNE   EXIT                                                             
*                                                                               
         GOTO1 VCOLY,BODMCB,('O#BILFRM',0),0,0                                  
         L     RF,BODMCB                                                        
         BASR  RE,RF                                                            
*                                                                               
         B     EXIT                EXIT WITH CC SET EQU/NEQ                     
         EJECT                                                                  
***********************************************************************         
* CLEAR AUTO REVERSE                                                  *         
***********************************************************************         
         SPACE 1                                                                
AUTCAR   L     R1,AIO3                                                          
         TM    TRNRSTA2-TRNRECD(R1),TRNSBILP                                    
         BO    AUTCAR02                                                         
         MVC   FVMSGNO,=AL2(AE$CCNAR)                                           
         B     EXITN               NOT AUTO REVERSED BILL                       
*                                                                               
AUTCAR02 LA    RF,TRNRFST-TRNRECD(R1)                                           
         USING RATELD,RF                                                        
         SR    R0,R0               SKIP STRAIGHT PAST TRNEL                     
AUTCAR04 IC    R0,RATLN                                                         
         AR    RF,R0                                                            
         CLI   RATEL,0                                                          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$CCNAR)                                           
         B     EXITN               NOT AUTO REVERSE BILL                        
         CLI   RATEL,RATETAXQ                                                   
         BNE   AUTCAR04                                                         
         OC    RATRATE,RATRATE                                                  
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$CCNAR)                                           
         B     EXITN               NOT AUTO REVERSE BILL                        
*                                                                               
         MVC   CSBILNUM,TLPPBIL    SET SELECTED BILL NUMBER                     
         MVC   CSBILCUR,TLPPCOD    SET SELECTED BILL CURRENCY CODE              
         MVC   CSEXCVAL,TLPPEXC    SET SELECTED BILL EXCHANGE RATE              
*                                                                               
         GOTO1 VCOLY,BODMCB,('O#BILARV',0),0,0                                  
         L     RF,BODMCB                                                        
         ICM   RF,8,OVACT          PASS SELECT TABLE NUMBER                     
         BASR  RE,RF                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CONVERT CURRENCY CODE INTO TABLE ENTRY                   *         
*                                                                     *         
* NTRY: P1=A(CURRENCY CODE)                                           *         
*       P2=A(CURRENCY TABLE ENTRY)                                    *         
* EXIT: CC=NOT EQUAL IF VBLDCUR CALLED                                *         
***********************************************************************         
         SPACE 1                                                                
CURTOTAB NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         CLC   CSCPYCUR,0(R2)                                                   
         BNE   *+14                                                             
         MVC   0(CURTABL,R3),CSCURCPY                                           
         B     EXITY                                                            
         CLC   CSBILCUR,0(R2)                                                   
         BNE   *+14                                                             
         MVC   0(CURTABL,R3),CSCURBIL                                           
         B     EXITY                                                            
*                                                                               
         LA    R4,CODTAB                                                        
         USING CODTABD,R4                                                       
CTAB02   CLI   CODTABD,EOT                                                      
         BE    CTAB04                                                           
         CLC   CODTCOD,0(R2)                                                    
         BNE   *+14                                                             
         MVC   0(L'CODTTAB,R3),0(R3)                                            
         B     EXITY                                                            
         LA    R4,CODTABL(R4)                                                   
         B     CTAB02                                                           
*                                                                               
CTAB04   GOTO1 VBLDCUR,BOPARM,CODTCOD,CODTTAB,ACOM                              
         MVC   0(L'CODTTAB,R3),CODTTAB                                          
         MVI   CODTABD+CODTABL,EOT                                              
         B     EXITN                                                            
         DROP  R4                                                               
         EJECT                                                                  
FF       EQU   X'FF'                                                            
ACCMST   DC    C'ACCMST '                                                       
REVWC    DC    C'99'                                                            
         SPACE 1                                                                
DEFCLM   DS    0XL1                * DEFAULT COLUMN LIST *                      
         DC    AL1(PRB#NET)                                                     
         DC    AL1(PRB#COM)                                                     
         DC    AL1(PRB#CUR)                                                     
         DC    AL1(EOT)                                                         
DEFCLML  EQU   *-DEFCLM                                                         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* OPTION TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
CLB0F    CSECT                                                                  
OPTTAB   DS    0X                                                               
*                                  DISPLAY=COLUMN CODES                         
         DC    AL2(UC8DSP-TWAD,UC3DSP-TWAD)                                     
         DC    AL1(OPTNRTN+OPTDFLTI,0)                                          
         DC    AL1(0,0,0,0,0,1,L'LSDISLST,L'LSDISLST)                           
         DC    AL1(0)                                                           
         DC    AL2(OPTDISQ,LSDIS-LSVALSD)                                       
         DC    CL4'+'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*     ?? (MORE)                                                                 
OPTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
*********************************************************************           
* VALIDATE OPTION INPUT                                             *           
*********************************************************************           
         SPACE 1                                                                
         DROP  R8,RB                                                            
         DS    0D                                                               
VALOPT   NMOD1 0,**VALO**                                                       
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         LR    R4,R2               R4=A(OPTTAB ENTRY)                           
         USING OPTTABD,R4                                                       
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *(RF)                                                            
         SPACE 1                                                                
VALX     XIT1  ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBCOLS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBCOLSC                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
       ++INCLUDE ACCLBWORKC                                                     
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBF1D                                                       
*                                                                               
         SPACE 1                                                                
WORKD    DSECT                                                                  
         ORG   OVERWRK                                                          
OVACT    DS    XL1                 SAVED SELECT ACTION                          
OBILAUTR DS    CL(L'CSBILNUM)      AUTO REVERSE BILL NUMBER                     
CODTAB   DS    0X                  SEE CODTABD                                  
         SPACE 1                                                                
CODTABD  DSECT                     ** CURRENCY CODE/TABLE TABLE **              
CODTCOD  DS    CL3                 CODE                                         
CODTTAB  DS    XL(CURTABL)         TABLE ENTRY                                  
CODTABL  EQU   *-CODTABD                                                        
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040ACCLB0F   08/16/00'                                      
         END                                                                    
