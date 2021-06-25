*          DATA SET ACREP8502  AT LEVEL 005 AS OF 05/01/02                      
*PHASE AC8502A,*                                                                
*INCLUDE ACCEDIT                                                                
         TITLE 'HISTORICAL ANALYSIS PROGRAM'                                    
         PRINT NOGEN                                                            
AC8502   CSECT                                                                  
         NMOD1 0,**AC8502,R9                                                    
         L     RA,0(,R1)                                                        
*                                                                               
         USING ACWORKD,RA                                                       
         USING ANALD,RC                                                         
*                                                                               
         LA    RC,SPACEND                                                       
         CLI   MODE,RUNFRST                                                     
         BNE   AN1                                                              
         SPACE 1                                                                
         LA    RE,RELOTAB          RELOCATE A-TYPES                             
         LA    R1,ATYPES                                                        
RELOOP   L     RF,0(,RE)                                                        
         A     RF,RELO                                                          
         ST    RF,0(,R1)                                                        
         LA    R1,4(,R1)                                                        
         LA    RE,4(,RE)                                                        
         CLI   0(RE),X'FF'                                                      
         BNE   RELOOP                                                           
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUFC                                      
         L     RF,AMONACC                                                       
*                                                                               
         USING ACMD,RF                                                          
*                                                                               
         MVC   ACLIST,ACMVALST                                                  
         B     ANEXT                                                            
*                                                                               
         DROP  RF                                                               
*                                                                               
AN1      DS    0H                                                               
         CLI   MODE,RUNLAST                                                     
         BE    ANEXT                                                            
         EJECT ,                                                                
***********************************************************************         
*  SWITCH TO APPROPRIATE ROUTINES                                               
***********************************************************************         
         SPACE 1                                                                
         CLI   MODE,REQFRST        NO, CONTINUE ON AS USUAL                     
         BE    AN12                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    AN13                                                             
         SR    R4,R4                                                            
         CLI   MODE,PROCACC                                                     
         BE    AN14                                                             
         CLI   MODE,PROCHIST                                                    
         BE    AN18                                                             
         CLI   MODE,OFFIRST                                                     
         BE    AN25                                                             
         CLI   MODE,OFACFRST                                                    
         BE    AN26                                                             
         CLI   MODE,PROCCBUK       TEST CAC BUCKET                              
         BE    AN28                YES                                          
*                                                                               
         CLI   QSEQ,QSEQDET        TEST FOR DETAIL SEQUENCE                     
         BNE   AN3                                                              
         CLI   MODE,OFACLAST                                                    
         BE    AN30                                                             
*                                                                               
AN3      LA    R3,7                                                             
         CLI   MODE,OFFLAST                                                     
         BE    AN38                                                             
*                                                                               
         LA    R3,3                                                             
         L     R4,ADACCNAM                                                      
         CLI   QSEQ,QSEQOFF                                                     
         BNE   AN5                                                              
         CLI   MODE,OFACLAST                                                    
         BE    AN32                                                             
         B     AN8                                                              
AN5      CLI   MODE,ACCLAST                                                     
         BE    AN32                                                             
*                                                                               
AN8      LA    R3,4                                                             
         L     R4,ADLVCNAM                                                      
         CLI   MODE,LEVCLAST                                                    
         BE    AN40                                                             
         LA    R3,5                                                             
         L     R4,ADLVBNAM                                                      
         CLI   MODE,LEVBLAST                                                    
         BE    AN40                                                             
         LA    R3,6                                                             
         L     R4,ADLVANAM                                                      
         CLI   MODE,LEVALAST                                                    
         BE    AN40                                                             
         LA    R3,9                                                             
         L     R4,ADCMPNAM                                                      
         CLI   MODE,REQLAST                                                     
         BE    AN40                                                             
******* OPTION TO SKIP TO A NEW PAGE UPON CHANGE OF LEVEL *********             
         CLI   PROGPROF,C'Y'       CHECK IF OPTION ACCEPTED                     
         BNE   ANEXT               NO - GET OUT                                 
         CLI   MODE,LEVAFRST                                                    
         BE    AN11                                                             
         CLI   MODE,LEVBFRST                                                    
         BE    AN11                                                             
         CLI   MODE,LEVCFRST                                                    
         BNE   ANEXT                                                            
AN11     MVI   FORCEHED,C'Y'       OPTION ACCEPTED                              
         SPACE 2                                                                
ANEXT    XMOD1 1                                                                
         SPACE 2                                                                
EXIT     XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*  REQUEST FIRST ROUTINES                                                       
***********************************************************************         
         SPACE 1                                                                
AN12     MVI   SWITCH,C'N'                                                      
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
*                                                                               
* INITIALIZE ACCMULATORS (10*13 6 BYTE PACKED)                                  
*                                                                               
         GOTO1 PROLLER,DMCB,0,ACCUMS,10,13                                      
         SPACE 3                                                                
         MVC   OPTLN,SPACES                                                     
         MVC   OPTLN,=C'BALANCES ANALYSED'                                      
         CLI   QOPT1,C'C'                                                       
         BNE   *+10                                                             
         MVC   OPTLN,=C'CREDITS ANALYSED '                                      
         CLI   QOPT1,C'D'                                                       
         BNE   *+10                                                             
         MVC   OPTLN,=C'DEBITS ANALYSED  '                                      
         MVC   DATELN,SPACES                                                    
         XC    MONTHLST,MONTHLST                                                
         LA    R6,12               MAX  12   MONTHS                             
         LA    R7,DATELN           ->   DATE LINE                               
         LA    R8,MONTHLST         ->   MONTHS LIST (PACKED)                    
         GOTO1 DATCON,DMCB,(0,QSTART),(0,WORK)                                  
         GOTO1 DATCON,DMCB,(0,QEND),(0,WORK+6)                                  
         MVC   WORK+4(2),=C'01'    MAKE START DD 01                             
         MVC   WORK+10(2),=C'01'   MAKE END   DD 01                             
         CLC   WORK(6),WORK+6      START DATE > END DATE                        
         BNH   *+6                 NO,  CONTINUE                                
         DC    H'00'               YES, BAD START/END DATE PAIR                 
*                                                                               
AN2      DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(6,(R7))            MMM/YY                  
         MVI   3(R7),C' '          REMOVE THE SLASH (/)                         
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+12)         PACKED YYMM01           
         MVC   0(2,R8),WORK+12     SAVE PACKED YYMM                             
         GOTO1 ADDAY,DMCB,(C'M',WORK),(0,WORK),F'1'     NEXT MONTH              
         CLC   WORK(6),WORK+6      ANY  MORE MONTHS TO GO ?                     
         BH    ANEXT               NO,  EXIT                                    
         LA    R7,11(,R7)                                                       
         LA    R8,2(,R8)                                                        
         BCT   R6,AN2              GET  NEXT MONTH                              
         DC    H'00'               TOO  MANY MONTHS                             
*                                                                               
         EJECT ,                                                                
***********************************************************************         
*  LEDGFRST LOGIC                                                               
***********************************************************************         
         SPACE 1                                                                
AN13     MVI   FORCEHED,C'Y'                                                    
         LA    R2,7                CLEAR FIRST 7 LEVELS OF ACCUMLATORS          
         SPACE 2                                                                
AN13A    GOTO1 PROLLER,DMCB,2,ACCUMS,(R2)                                       
         BCT   R2,AN13A                                                         
         B     ANEXT                                                            
         EJECT ,                                                                
***********************************************************************         
*  PROCACC LOGIC                                                                
***********************************************************************         
         SPACE 1                                                                
AN14     XC    OFFICES,OFFICES     CLEAR COUNT OF OFFICES                       
         B     ANEXT                                                            
         EJECT ,                                                                
***********************************************************************         
*  PROCESS HISTORY ELEMENTS                                                     
***********************************************************************         
         SPACE 1                                                                
         USING TRHISTD,R2                                                       
         SPACE 1                                                                
AN18     L     R2,ADTRANS          WE HAVE A HISTORY RECORD                     
         CLI   TRHSEL,X'45'        JUST DOUBLE CHECK IT                         
         BNE   ANEXT               NO, THIS SHOULDN'T HAPPEN                    
         CLC   BUCKTYPE,QOPT3      WE HAVE ONE, CHECK THIS OUT                  
         BNE   ANEXT                                                            
         SPACE 1                                                                
         BAS   RE,CLIFILT          FILTER CLIENT                                
         CLI   WANT,C'N'                                                        
         BE    ANEXT                                                            
*                                                                               
         USING TRHISTD,R2                                                       
*                                                                               
AN19     ICM   R2,15,ADTRANS                                                    
         BZ    ANEXT                                                            
         LA    R3,MONTHLST         GET A MATCH ON PLACE IN LIST                 
         LA    R4,1                                                             
         SPACE 2                                                                
AN20     CLC   TRHSYEAR(2),0(R3)   PACKED YYMM                                  
         BE    AN22                                                             
         LA    R3,2(R3)                                                         
         LA    R4,1(R4)                                                         
         CH    R4,=H'13'                                                        
         BNL   ANEXT                                                            
         B     AN20                                                             
         SPACE 2                                                                
AN22     ZAP   DUB,TRHSDR          NOW ESTABLISH POSTING AMOUNT                 
         CLI   QOPT1,C'D'                                                       
         BE    AN24                                                             
         SP    DUB,TRHSCR                                                       
         CLI   QOPT1,C'C'                                                       
         BNE   AN24                                                             
         ZAP   DUB,TRHSCR                                                       
         SPACE 2                                                                
* ADD 6-BYTE PACKED NUMBER AT DUB+2 TO LINE 1 AND COLUMN IN R4                  
*                                                                               
AN24     GOTO1 PROLLER,DMCB,3,ACCUMS,DUB+2,1,(R4)                               
         B     ANEXT                                                            
         EJECT ,                                                                
***********************************************************************         
*  OFFIRST - FIRST FOR OFFICE  (OFFICE SEQUENCE)                                
***********************************************************************         
         SPACE 1                                                                
         USING ACMD,RF                                                          
         USING OFARECD,R2                                                       
         SPACE 1                                                                
AN25     CLI   QSEQ,QSEQOFF                                                     
         BNE   AN25X                                                            
*                                                                               
         L     RF,AMONACC                                                       
         MVC   OFFNAME,ACMOFNAM    EXTRACT OFFICE NAME                          
         L     R2,ACMAOFA                                                       
         MVC   OFFCODE,OFAKOFF     EXTRACT OFFICE CODE                          
*                                                                               
         MVI   FORCEHED,C'Y'       BREAK A PAGE                                 
*                                                                               
         GOTO1 PROLLER,DMCB,2,ACCUMS,7  CLEAR LINE 7                            
*                                                                               
         MVI   OFFSW,C'N'                                                       
*                                                                               
AN25X    B     ANEXT                                                            
         DROP  R2,RF                                                            
         EJECT ,                                                                
***********************************************************************         
*  OFACFRST - FIRST FOR OFFICE/ACCOUNT (DETAIL SEQUENCE)                        
***********************************************************************         
         SPACE 1                                                                
         USING ACMD,RF                                                          
         USING BUCKBLKD,R2                                                      
         SPACE 1                                                                
AN26     L     RF,AMONACC                                                       
         L     R2,ACMABUCK                                                      
         MVC   BUCKFILT,QOPT3                                                   
*                                                                               
         CLI   QSEQ,QSEQDET        TEST FOR DETAIL SEQUENCE                     
         BNE   AN26X               NO - ALL DONE                                
*                                                                               
         MVC   OFFNAME,ACMOFNAM    EXTRACT OFFICE NAME                          
         L     R2,ACMAOFA                                                       
*                                                                               
         USING OFARECD,R2                                                       
*                                                                               
         MVC   OFFCODE,OFAKOFF     EXTRACT OFFICE CODE                          
*                                                                               
         LA    R0,13                                                            
         LA    RE,OFFTOTS          CLEAR OFFICE TOTALS                          
         ZAP   0(L'OFFTOTS,RE),=P'0'                                            
         LA    RE,L'OFFTOTS(RE)                                                 
         BCT   R0,*-10                                                          
*                                                                               
         MVI   OFFSW,C'N'          SET OFFICE POSTED SWITCH TO NO               
*                                                                               
AN26X    B     ANEXT                                                            
         DROP  R2,RF                                                            
         EJECT ,                                                                
***********************************************************************         
* PROCCBUK - PROCESS AN ACCOUNT/OFFICE/CAC BUCKET                               
***********************************************************************         
         SPACE 1                                                                
AN28     BAS   RE,CLIFILT                                                       
         CLI   WANT,C'Y'           TEST WANT THIS CAC                           
         BNE   *+8                 NO                                           
         BAS   RE,BUCKET           YES-HANDLE BUCKET RECORD                     
         B     ANEXT                                                            
         SPACE 2                                                                
*              OFACLAST - LAST TIME FOR OFFICE/ACCOUNT                          
         SPACE 1                                                                
AN30     CLI   QSEQ,QSEQDET        TEST FOR DETAIL SEQUENCE                     
         BNE   AN30X                                                            
*                                                                               
         CLI   OFFSW,C'Y'          TEST ANY ACTIVITY FOR OFFICE                 
         BNE   AN30X               NO                                           
*                                                                               
         CLC   OFFTOTS(12*L'OFFTOTS),=13PL6'0'                                  
         BE    AN30X                                                            
         OC    OFFICES,OFFICES     TEST FOR FIRST OFFICE                        
         BNZ   AN30A                                                            
*                                                                               
         BAS   RE,ACNAME           YES-PRINT THE A/C NAME FIRST                 
         ZIC   R1,LINE             TEST IF NAME + 1 LINE                        
         LA    R1,2(R1)            FIT ON PAGE                                  
         CLC   PSECOND,SPACES      TEST FOR 2 LINE NAME                         
         BE    *+8                                                              
         LA    R1,1(R1)            YES                                          
         CLM   R1,1,MAXLINES                                                    
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         BAS   RE,PRNTIT                                                        
*                                                                               
AN30A    BAS   RE,OFFDET           YES-PRINT DETAIL                             
*                                                                               
AN30X    B     ANEXT                                                            
         EJECT ,                                                                
***********************************************************************         
*  SPECIAL LAST TIME ROUTINES                                                   
*                                                                               
*    ACCLAST LOGIC                                                              
*                                                                               
***********************************************************************         
         SPACE 1                                                                
AN32     GOTO1 PROLLER,DMCB,6,ACCUMS    ROLL UP LINE 1                          
         GOTO1 PROLLER,DMCB,2,,1        CLEAR LINE 1                            
         GOTO1 FORMAT,DMCB,(R3)                                                 
         CLC   P+32(78),SPACES                                                  
         BNE   *+14                                                             
         CLC   PSECOND+32(78),SPACES                                            
         BE    ANEXT                                                            
*                                                                               
AN34     CLI   QSEQ,QSEQDET        TEST FOR DETAIL SEQUENCE                     
         BE    AN35                YES                                          
         BAS   RE,ACNAME           SET UP ACCOUNT NAME                          
         B     AN36                                                             
*                                                                               
AN35     MVC   P+2(8),=C'*TOTALS*'                                              
         CLC   OFFICES,=H'1'       SUPPRESS REDUNDANT TOTAL LINE                
         BH    AN36                NEED TOTAL FOR MORE THAN 1 OFFICE            
*                                                                               
         MVC   P,SPACES            CLEAR PRINT LINES                            
         MVC   PSECOND,SPACES                                                   
         B     ANEXT                                                            
*                                                                               
AN36     DS    0H                                                               
         BAS   RE,PRNTIT                                                        
         MVI   SWITCH,C'Y'                                                      
         B     ANEXT                                                            
         EJECT ,                                                                
***********************************************************************         
*                                                                               
*    OFFLAST ROUTINE - OFFICE SEQUENCE                                          
*                                                                               
***********************************************************************         
         SPACE 1                                                                
AN38     CLI   QSEQ,QSEQOFF        TEST OFFICE SEQUENCE                         
         BNE   AN38X                                                            
*                                                                               
         GOTO1 PROLLER,DMCB,1,ACCUMS,(R3)                                       
         L     R2,DMCB             GET A(OFFICE TOTALS)                         
*                                                                               
AN38A    XC    BUFFREC(BUFFRECQ),BUFFREC BUILD BUFFALO RECORD                   
         MVI   BUFFLEVL,X'FE'                                                   
         MVC   BUFFOFFC,OFFCODE                                                 
         MVC   BUFFOFFN,OFFNAME                                                 
         LA    R0,13                                                            
         LA    RE,BUFFACCS                                                      
*                                                                               
         ZAP   0(L'BUFFACCS,RE),0(L'OFFTOTS,R2)                                 
         LA    R2,L'OFFTOTS(R2)                                                 
         LA    RE,L'BUFFACCS(RE)                                                
         BCT   R0,*-14                                                          
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFFREC                              
*                                                                               
         MVI   BUFFLEVL,X'FF'                                                   
         XC    BUFFOFFC,BUFFOFFC                                                
         MVC   BUFFOFFN,SPACES                                                  
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFFREC                              
*                                                                               
AN38B    MVC   CHUNK,SPACES                                                     
         MVC   CHUNK(10),=C'TOTALS FOR'                                         
         MVC   CHUNK+11(L'OFFNAME),OFFNAME                                      
         GOTO1 CHOPPER,DMCB,(50,CHUNK),(31,P+1),(C'P',2)                        
         LA    R3,255              SIGNAL BUFFALO RECORD                        
         GOTO1 FORMAT,DMCB,('FORMZER',(R3))                                     
         BAS   RE,PRNTIT                                                        
*                                                                               
AN38C    MVC   OFFCODE,SPACES      CLEAR OFFICE CODE/NAME                       
         MVC   OFFNAME,SPACES                                                   
*                                                                               
AN38X    B     ANEXT                                                            
         EJECT ,                                                                
***********************************************************************         
*  GENERAL LAST TIME ROUTINES                                                   
*                                                                               
*    LEVCLAST,LEVBLAST,LEVALAST, AND REQLAST                                    
*                                                                               
***********************************************************************         
         SPACE 1                                                                
         USING ACHEIRD,R2                                                       
         SPACE 1                                                                
AN40     L     R2,ADLDGHIR                                                      
         MVC   P,SPACES                                                         
         GOTO1 PROLLER,DMCB,1,ACCUMS,(R3)  GET A(LINE IN R3)                    
         L     R1,DMCB                                                          
         CLC   0(78,R1),=13PL6'0'                                               
         BE    AN46                                                             
         MVI   FORCEHED,C'N'                                                    
         MVC   TOTAB+15(15),ACHRDESC                                            
         MVC   TOTAB+30(15),ACHRDESB                                            
         MVC   TOTAB+45(15),ACHRDESA                                            
         CLI   SWITCH,C'Y'                                                      
         BNE   AN42                                                             
         MVI   SWITCH,C'N'                                                      
         BAS   RE,PRNTIT                                                        
         SPACE 2                                                                
AN42     GOTO1 FORMAT,DMCB,(R3)                                                 
         CLC   P+32(78),SPACES                                                  
         BE    ANEXT                                                            
         MVC   CHUNK,SPACES                                                     
         MVC   CHUNK(10),=C'TOTALS FOR'                                         
         SH    R3,=H'3'                                                         
         MH    R3,=H'15'                                                        
         LA    R3,TOTAB(R3)                                                     
         MVC   CHUNK+11(15),0(R3)                                               
         CLC   0(7,R3),=C'REQUEST'                                              
         BE    AN43                                                             
         LTR   R4,R4                                                            
         BZ    AN44                                                             
         LR    R2,R4                                                            
*                                                                               
         USING ACNAMED,R2                                                       
*                                                                               
         SR    R4,R4                                                            
         IC    R4,ACNMLEN                                                       
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   CHUNK+27(0),ACNMNAME                                             
         SPACE 2                                                                
AN43     GOTO1 ADSQUASH,DMCB,CHUNK,70                                           
         XC    DMCB,DMCB                                                        
         GOTO1 CHOPPER,DMCB,(70,CHUNK),(31,P+1),(C'P',2)                        
         MVC   CHUNK,SPACES                                                     
         SPACE 2                                                                
AN44     DS    0H                                                               
         CLI   MODE,REQLAST                                                     
         BNE   AN44A                                                            
         MVC   HEAD7,SPACES                                                     
         B     AN45                                                             
AN44A    CLI   MODE,LEVALAST                                                    
         BNE   AN45A                                                            
         CLI   PROGPROF,C'Y'                                                    
         BNE   AN45A                                                            
AN45     MVI   FORCEHED,C'Y'                                                    
AN45A    MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
AN46     CLI   MODE,REQLAST                                                     
         BNE   ANEXT                                                            
*                                                                               
         MVI   RCSUBPRG,1                                                       
         BAS   RE,RECAP                                                         
         MVI   RCSUBPRG,0                                                       
         GOTO1 BUFFALO,DMCB,=C'RESET',ADBUFC                                    
         B     ANEXT                                                            
         SPACE 2                                                                
TOTAB    DC    CL15'ACCOUNT'                                                    
         DC    CL45' '                                                          
         DC    CL15'LEDGER'                                                     
         DC    CL15'UNIT'                                                       
         DC    CL15'REQUEST'                                                    
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO FORMAT ACCOUNT NAME INTO P (AND P2)                               
***********************************************************************         
         SPACE 1                                                                
ACNAME   NTR1  ,                                                                
         MVC   CHUNK,SPACES                                                     
         GOTO1 ACCEDIT,DMCB,ADACC,ADLDGHIR,CHUNK                                
         L     R2,ADACCNAM                                                      
*                                                                               
         USING ACNAMED,R2                                                       
*                                                                               
         SR    R4,R4                                                            
         MVC   WORK,SPACES                                                      
         IC    R4,ACNMLEN                                                       
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ACNMNAME                                                 
*                                                                               
         MVC   CHUNK+20(36),WORK                                                
         OC    CHUNK,SPACES                                                     
         CLC   CHUNK,SPACES                                                     
         BE    ACNAMEX                                                          
         GOTO1 ADSQUASH,DMCB,CHUNK,60                                           
         XC    DMCB,DMCB                                                        
         GOTO1 CHOPPER,DMCB,(60,CHUNK),(31,P+1),(C'P',2)                        
*                                                                               
ACNAMEX  B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  PRINT ROUTINE                                                                
***********************************************************************         
         SPACE 1                                                                
PRNTIT   NTR1  ,                                                                
         CLI   RCSUBPRG,1          TEST FOR OFFICE RECAP                        
         BE    PRNTIT2             YES                                          
*                                                                               
         CLI   QSEQ,QSEQDET                                                     
         BNE   *+10                                                             
         MVC   HEAD4+47(18),=C'WITH OFFICE DETAIL'                              
         CLI   QSEQ,QSEQOFF                                                     
         BNE   *+10                                                             
         MVC   HEAD4+51(9),=C'BY OFFICE'                                        
*                                                                               
PRNTIT2  MVC   HEAD6+85(17),OPTLN                                               
         MVC   HEAD10+37(66),DATELN                                             
         MVC   HEAD11+37(66),DATELN+66                                          
*                                                                               
PRNTIT3  CLI   QSEQ,QSEQOFF        TEST OFFICE SEQUENCE                         
         BNE   PRNTIT4                                                          
         MVC   HEAD7+85(2),OFFCODE                                              
         MVC   HEAD7+88(21),OFFNAME  ONLY ROOM FOR 21 CHARS                     
*                                                                               
PRNTIT4  GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  FORMAT AND CLEAR A LINE OF ACCUMULATORS                                      
***********************************************************************         
         SPACE 1                                                                
FORMAT   NTR1  ,                                                                
         L     R3,0(R1)            P1=ACCUMULATOR NUMBER                        
         LA    R3,0(R3)            CLEAR HOB                                    
         MVC   FORMOPTS,0(R1)      SAVE OPTIONS FLAGS                           
*                                                                               
         LA    R2,OFFTOTS                                                       
         CH    R3,=H'254'                                                       
         BL    FORMAT1                                                          
         BE    FORMAT2                                                          
*                                                                               
         LA    RE,BUFFACCS         FOR A BUFFALO CALL-CONVERT TO 6 BYTE         
         LA    R0,13               NUMBERS IN OFFTOTS                           
         ZAP   0(L'OFFTOTS,R2),0(L'BUFFACCS,RE)                                 
         LA    R2,L'OFFTOTS(R2)                                                 
         LA    RE,L'BUFFACCS(RE)                                                
         BCT   R0,*-14                                                          
         LA    R2,OFFTOTS                                                       
         B     FORMAT2                                                          
*                                                                               
FORMAT1  GOTO1 PROLLER,DMCB,1,ACCUMS,(R3)                                       
         L     R2,DMCB                                                          
*                                                                               
FORMAT2  MVC   CHUNK,SPACES                                                     
         LA    R3,CHUNK                                                         
         MVC   CHUNK2,SPACES                                                    
         LA    R4,13                                                            
         SPACE 2                                                                
FORMAT4  CH    R4,=H'1'                                                         
         BNE   *+8                                                              
         LA    R3,P+99                                                          
         CP    0(6,R2),=P'0'       TEST FOR ZERO ACCUM                          
         BNE   FORMAT4A                                                         
         TM    FORMOPTS,FORMZER    YES-TEST IF CALLER WANTS TO PRINT IT         
         BZ    FORMAT8                                                          
*                                                                               
         CH    R4,=H'1'            TEST FOR TOTAL COLUMN                        
         BE    FORMAT4A            YES-ALWAYS WANT TO PRINT IT                  
         LA    R1,13                                                            
         SR    R1,R4               COMPUTE INDEX TO MONTH                       
         SLL   R1,1                X 2 OR LENGTH OF MONTHLST ENTRY              
         LA    R1,MONTHLST(R1)                                                  
         OC    0(2,R1),0(R1)       TEST IF MONTH IS ON THE REPORT               
         BZ    FORMAT8             NO-SKIP SHOWING THE ZERO                     
*                                                                               
FORMAT4A CLI   QOPT5,C'Y'          OPTION FOR BIG NUMBERS                       
         BE    FORMAT5                                                          
         EDIT  (P6,(R2)),(11,0(R3)),2,FLOAT=-,ZERO=NOBLANK                      
         B     FORMAT6                                                          
         SPACE 1                                                                
FORMAT5  ZAP   DIV,0(6,R2)         ROUND,DROP PENNIES AND EDIT                  
         AP    DIV,=P'50'                                                       
         CP    DIV,=P'0'                                                        
         BNL   *+10                                                             
         SP    DIV,=P'100'                                                      
         DP    DIV,=P'100'                                                      
         EDIT  (P8,DIV),(11,0(R3)),FLOAT=-,ZERO=NOBLANK                         
         SPACE 2                                                                
FORMAT6  ZAP   0(6,R2),=P'0'                                                    
         SPACE 2                                                                
FORMAT8  LA    R2,6(R2)                                                         
         LA    R3,11(R3)                                                        
         BCT   R4,FORMAT4                                                       
         MVC   P+32(66),CHUNK                                                   
         MVC   PSECOND+32(66),CHUNK+66                                          
         CLC   PSECOND,SPACES                                                   
         BE    *+8                                                              
         MVI   SPACING,2                                                        
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO FILTER CLIENTS                                                    
***********************************************************************         
         SPACE 1                                                                
         USING TRSUBHD,R2                                                       
         SPACE 1                                                                
CLIFILT  NTR1                                                                   
         MVI   WANT,C'Y'                                                        
         CLC   QSELECT,SPACES                                                   
         BE    EXIT                NO FILTERS                                   
         L     R2,ADSUBAC                                                       
         MVC   THREE,TRSBACNT+12                                                
         CLI   QLEDGER,C'V'                                                     
         BNE   *+10                                                             
         MVC   THREE,TRSBACNT+3                                                 
         CLI   QLEDGER,C'W'                                                     
         BNE   *+10                                                             
         MVC   THREE,TRSBACNT+3                                                 
*                                                                               
         USING RUNXTRAD,RF                                                      
*                                                                               
         L     RF,VEXTRAS                                                       
         OC    VLISTREC,VLISTREC                                                
         BZ    CLIFILT5            NOT LIST RECORD                              
         GOTO1 ACLIST,DMCB,VLISTREC,THREE                                       
         CLI   DMCB,0                                                           
         BNE   *+6                                                              
         DC    H'0'                BAD LIST RECORD                              
         CLI   DMCB,C'E'                                                        
         BNE   EXIT                DON'T EXCLUDE THIS CLIENT                    
         MVI   WANT,C'N'           EXCLUDE CLIENT                               
         B     EXIT                                                             
         SPACE 1                                                                
CLIFILT5 CLC   THREE,QSELECT                                                    
         BE    EXIT                WANT THIS CLIENT                             
         TM    QSELECT,X'40'       'NOT' LOGIC                                  
         BO    CLIFILTN                                                         
         NI    THREE,X'BF'                                                      
         CLC   THREE,QSELECT       CAN WE EXCLUDE THIS CLIENT                   
         BE    CLIFILTN            IF EQUAL EXCLUDE                             
         B     CLIFILTY                                                         
CLIFILTN MVI   WANT,C'N'            EXCLUDE THIS CLIENT                         
CLIFILTY OI    THREE,X'40'                                                      
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO HANDLE CAC BUCKET RECORDS                                         
***********************************************************************         
         SPACE 1                                                                
         USING ACMD,R3                                                          
         USING BUKELD,R4                                                        
         SPACE 1                                                                
BUCKET   NTR1  ,                                                                
         L     R3,AMONACC                                                       
         L     R4,ACMABUK                                                       
         AH    R4,DATADISP                                                      
*                                                                               
BUCKET2  CLI   BUKEL,0                                                          
         BE    BUCKETX                                                          
         CLI   BUKEL,BUKELQ                                                     
         BE    BUCKET4                                                          
*                                                                               
BUCKET3  ZIC   R0,BUKLN                                                         
         AR    R4,R0                                                            
         B     BUCKET2                                                          
*                                                                               
BUCKET4  LA    R2,MONTHLST         R2=A(YEAR/MONTH)                             
         LA    R5,OFFTOTS          R5=A(OFFICE BUCKETS)                         
         LA    R1,12               R1=LOOP COUNTER                              
         LA    RE,1                RE=COLUMN NUMBER                             
*                                                                               
BUCKET5  CLC   BUKMOS,0(R2)        MATCH ON MOS                                 
         BE    BUCKET6             YES                                          
         LA    R2,2(R2)                                                         
         LA    R5,L'OFFTOTS(R5)                                                 
         LA    RE,1(RE)                                                         
         BCT   R1,BUCKET5                                                       
         B     BUCKET3             NOT IN REPORT PERIOD-NEXT ELEM               
*                                                                               
BUCKET6  ZAP   DUB,BUKDR                                                        
         CLI   QOPT1,C'D'          TEST REPORTING DEBITS                        
         BE    BUCKET8             YES                                          
         SP    DUB,BUKCR                                                        
         CLI   QOPT1,C'B'          TEST REPORTING BALANCES                      
         BE    BUCKET8             YES                                          
         ZAP   DUB,BUKCR           NO-SHOWING CREDITS                           
*                                                                               
BUCKET8  CLI   QSEQ,QSEQDET        TEST DETAIL SEQUENCE                         
         BE    BUCKET9                                                          
         LR    R2,RE               SHIFT COLUMN NUMBER                          
         GOTO1 PROLLER,DMCB,3,ACCUMS,DUB+2,1,(R2)                               
         B     BUCKET10                                                         
*                                                                               
BUCKET9  AP    0(L'OFFTOTS,R5),DUB UPDATE OFFICE BUCKET                         
*                                                                               
BUCKET10 MVI   OFFSW,C'Y'                                                       
         B     BUCKET3             NEXT ELEMENT                                 
*                                                                               
BUCKETX  B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO HANDLE OFFICE DETAIL                                              
***********************************************************************         
         SPACE 1                                                                
OFFDET   NTR1  ,                                                                
         LH    R1,OFFICES          UPDATE COUNT OF OFFICE FOR ACCOUNT           
         LA    R1,1(R1)                                                         
         STH   R1,OFFICES                                                       
*                                                                               
OFFDET2  GOTO1 PROLLER,DMCB,1,ACCUMS,1                                          
         L     R2,DMCB             R2=A(ACCOUNT TOTALS)                         
         LA    R3,OFFTOTS          R3=A(OFFICE TOTALS)                          
         LA    R0,12               R0=COUNTER                                   
         ZAP   DUB,=P'0'           INITIALIZE TOTAL BUCKET                      
*                                                                               
OFFDET3  AP    0(6,R2),0(L'OFFTOTS,R3)  UPDATE ACCOUNT TOTALS                   
         AP    DUB,0(L'OFFTOTS,R3)                                              
         LA    R2,6(R2)                                                         
         LA    R3,L'OFFTOTS(R3)                                                 
         BCT   R0,OFFDET3                                                       
*                                                                               
         ZAP   0(L'OFFTOTS,R3),DUB UPDATE OFFICE TOTAL COL                      
*                                                                               
OFFDET4  MVI   BUFFLEVL,X'FE'      OFFICE TOTAL RECORD                          
         MVC   BUFFOFFC,OFFCODE                                                 
         MVC   BUFFOFFN,OFFNAME                                                 
         LA    R2,BUFFACCS                                                      
         LA    R3,OFFTOTS                                                       
         LA    R0,13                                                            
*                                                                               
         ZAP   0(L'BUFFACCS,R2),0(L'OFFTOTS,R3)                                 
         LA    R2,L'BUFFACCS(R2)                                                
         LA    R3,L'OFFTOTS(R3)                                                 
         BCT   R0,*-14                                                          
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFFREC                              
*                                                                               
         MVI   BUFFLEVL,X'FF'                                                   
         XC    BUFFOFFC,BUFFOFFC                                                
         MVC   BUFFOFFN,SPACES                                                  
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFFREC                              
*                                                                               
OFFDET6  LA    R3,254              USE OFFTOTS                                  
         GOTO1 FORMAT,DMCB,(R3)                                                 
         MVC   CHUNK,SPACES                                                     
         MVC   CHUNK(L'OFFCODE),OFFCODE                                         
         MVC   CHUNK+L'OFFCODE+2(L'OFFNAME),OFFNAME                             
         GOTO1 CHOPPER,DMCB,(50,CHUNK),(28,P+3),(C'P',2)                        
         BAS   RE,PRNTIT                                                        
*                                                                               
OFFDETX  B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO PRINT OFFICE RECAP                                                
***********************************************************************         
         SPACE 1                                                                
RECAP    NTR1  ,                                                                
         L     RE,AMONACC                                                       
         TM    ACMINDS-ACMD(RE),ACMIEMUD+ACMINEWO                               
         BNO   RECAPX                                                           
         CLI   QSEQ,QSEQOFF        OFFICE SEQUENCE OR. . .                      
         BE    *+12                                                             
         CLI   QSEQ,QSEQDET        . . . OFFICE DETAIL?                         
         BNE   RECAPX                                                           
*                                                                               
RECAP2   MVI   FORCEHED,C'Y'                                                    
         XC    BUFFREC(BUFFRECQ),BUFFREC                                        
         MVI   BUFFLEVL,X'FE'      GET OFFICE TOTALS FOR THE LEDGER             
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFFREC,1                           
*                                                                               
RECAP4   TM    DMCB+8,X'90'        ANYTHING TO PRINT?                           
         BNZ   RECAPX              NO                                           
         CLI   BUFFLEVL,X'FE'      STILL LOOKING AT OFFICE TOTALS?              
         BNE   RECAP6                                                           
         SPACE                                                                  
         MVI   SPACING,2                                                        
         MVC   CHUNK,SPACES                                                     
         MVC   CHUNK(2),BUFFOFFC   OFFICE CODE                                  
         MVC   CHUNK+4(L'BUFFOFFN),BUFFOFFN                                     
         GOTO1 CHOPPER,DMCB,(40,CHUNK),(31,P+1),(C'P',2)                        
         LA    R3,255                                                           
         GOTO1 FORMAT,DMCB,(R3)                                                 
         BAS   RE,PRNTIT                                                        
         GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFFREC,1                            
         B     RECAP4                                                           
         SPACE 1                                                                
RECAP6   CLI   BUFFLEVL,X'FF'      LEDGER TOTAL RECORD?                         
         BNE   RECAPX              BUFFALO LEDGER TOTAL RECORD VANISHED         
         SPACE 1                                                                
         MVC   CHUNK,SPACES                                                     
         MVC   CHUNK+1(10),=C'TOTALS FOR'                                       
         L     R3,ADLDGNAM                                                      
*                                                                               
         USING ACNAMED,R3                                                       
*                                                                               
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CHUNK+12(0),ACNMNAME                                             
         GOTO1 CHOPPER,DMCB,(60,CHUNK),(31,P+1),(C'P',2)                        
         LA    R3,255                                                           
         GOTO1 FORMAT,DMCB,(R3)                                                 
         BAS   RE,PRNTIT                                                        
*                                                                               
RECAPX   B     EXIT                                                             
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
*  WORK SPACE ETC., FOR MODULE                                                  
***********************************************************************         
         SPACE 1                                                                
RELOTAB  DS    0A                                                               
         DC    V(BUFFALOC)                                                      
         DC    V(ACCEDIT)                                                       
         DC    X'FF'                                                            
         EJECT ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR MODULE                                                             
***********************************************************************         
         SPACE 1                                                                
ANALD    DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
ADBUFC   DS    V                                                                
ACCEDIT  DS    V                                                                
ACLIST   DS    V                                                                
         SPACE 1                                                                
ACCUMS   DS    788C                10X13X6 +8                                   
SWITCH   DS    CL1                                                              
FORMOPTS DS    X                   FORMAT SUB-ROUTINE OPTIONS                   
FORMZER  EQU   X'80'               SHOW ZERO AMOUNTS                            
DIV      DS    PL10                                                             
MONTHLST DS    CL26                12 PACKED YYMM DATES                         
CHUNK    DS    CL132                                                            
CHUNK2   DS    CL132                                                            
OFFICES  DS    H                   N'OFFICES WITHIN ACCOUNT                     
OFFSW    DS    CL1                                                              
OFFCODE  DS    CL2                                                              
OFFNAME  DS    CL36                                                             
OFFTOTS  DS    13PL6                                                            
*                                                                               
BUFFREC  DS    0D                                                               
BUFFLEVL DS    XL1                 SORT LEVEL                                   
*                                  X'FE'=OFFICE TOTALS FOR LEDGER               
*                                  X'FF'=LEDGER TOTALS                          
BUFFOFFC DS    CL2                 OFFICE CODE                                  
BUFFOFFN DS    CL36                OFFICE NAME                                  
BUFFACCS DS    13PL8               MONTHLY ACCUMULATORS                         
BUFFRECQ EQU   *-BUFFREC                                                        
         SPACE 1                                                                
DATELN   DS    CL132               12 CHARACTER MMM YY COLUMNS                  
OPTLN    DS    CL17                                                             
WANT     DS    CL1                                                              
ANALX    EQU   *                                                                
         EJECT ,                                                                
         SPACE 1                                                                
         BUFF  LINES=256,ROWS=1,COLUMNS=13,FLAVOR=PACKED,              X        
               KEYLIST=(3,A),COMMENT=36                                         
         SPACE 1                                                                
*DDBUFFALOD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
         PRINT ON                                                               
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*ACBUCKETD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACBUCKETD                                                      
         PRINT ON                                                               
*ACMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*DDREPXTRAD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREP8502 05/01/02'                                      
         END                                                                    
