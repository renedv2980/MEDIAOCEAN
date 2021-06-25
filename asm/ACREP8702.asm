*          DATA SET ACREP8702  AT LEVEL 073 AS OF 05/01/02                      
*PHASE AC8702A                                                                  
*INCLUDE ACCEDIT                                                                
         TITLE 'HISTORICAL CROSS-ANALYSIS PROGRAM'                              
         PRINT NOGEN                                                            
AC8702   CSECT                                                                  
         NMOD1 0,**AC8702,R9                                                    
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
*                                                                               
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
         CLI   MODE,PROCRQST       INSPECT THE REQUEST                          
         BE    AN10                                                             
         CLI   MODE,REQFRST                                                     
         BE    AN12                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    AN13                                                             
         SR    R4,R4                                                            
         CLI   MODE,SBACFRST                                                    
         BE    AN26                                                             
         CLI   MODE,ACCFRST                                                     
         BE    AN26                                                             
         CLI   MODE,PROCACC                                                     
         BE    AN14                                                             
         CLI   MODE,PROCHIST                                                    
         BE    AN18                                                             
         CLI   MODE,PROCCBUK                                                    
         BE    AN29                                                             
         CLI   MODE,OFFIRST                                                     
         BE    AN25                                                             
         CLI   MODE,OFACFRST                                                    
         BE    AN27                                                             
*                                                                               
         LA    R3,2                                                             
         CLI   MODE,SBACLAST                                                    
         BE    AN30                                                             
         LA    R3,7                                                             
         CLI   MODE,OFFLAST                                                     
         BE    AN39                                                             
         LA    R3,3                                                             
         L     R4,ADACCNAM                                                      
         CLI   MODE,ACCLAST                                                     
         BE    AN38                                                             
         LA    R3,4                                                             
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
*                                                                               
******* OPTION TO SKIP TO A NEW PAGE UPON CHANGE OF LEVEL *********             
         CLI   PROGPROF,C'Y'       CHECK IF OPTION ACCEPTED                     
         BNE   ANEXT               NO - GET OUT                                 
         CLI   MODE,LEVAFRST                                                    
         BE    AN11                                                             
         CLI   MODE,LEVBFRST                                                    
         BE    AN11                                                             
         CLI   MODE,LEVCFRST                                                    
         BNE   ANEXT                                                            
*                                                                               
AN11     MVI   FORCEHED,C'Y'       OPTION ACCEPTED                              
         SPACE 2                                                                
ANEXT    XMOD1 1                                                                
         EJECT ,                                                                
***********************************************************************         
*  PROCRQST ROUTINE - GENERATE BUCKETS FOR OFFICE SEQUENCES                     
***********************************************************************         
         SPACE 1                                                                
AN10     MVI   FCGENBUK,C'N'       DON'T GENERATE BUCKETS                       
         L     RE,AMONACC                                                       
         TM    ACMINDS-ACMD(RE),ACMIEMUD+ACMINEWO                               
         BNO   *+8                                                              
         MVI   FCGENBUK,FCGENCON   SET TO GENERATE CONTRA BUCKETS               
*                                                                               
         B     ANEXT                                                            
         EJECT ,                                                                
***********************************************************************         
*  FIRST TIME ROUTINES                                                          
***********************************************************************         
         SPACE 1                                                                
AN12     GOTO1 BUFFALO,DMCB,=C'RESET',ADBUFC                                    
         MVI   SUMMFLAG,0                                                       
         MVI   SWITCH,C'N'         REQUEST FIRST                                
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
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
         SPACE 1                                                                
AN14     MVI   FORCEHED,C'Y'       ACCOUNT FIRST                                
         MVC   HEAD7+1(7),=C'ACCOUNT'                                           
         GOTO1 ACCEDIT,DMCB,ADACC,ADLDGHIR,HEAD7+12                             
         MVC   SAVACC,HEAD7+1                                                   
         B     ANEXT                                                            
         SPACE 2                                                                
AN13     MVI   FORCEHED,C'Y'       LEDGER FIRST                                 
         LA    R2,6                                                             
         SPACE 2                                                                
AN13A    GOTO1 PROLLER,DMCB,2,ACCUMS,(R2)                                       
         BCT   R2,AN13A                                                         
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
         L     R2,ADTRANS                                                       
         OC    ADTRANS,ADTRANS                                                  
         BZ    ANEXT                                                            
         LA    R3,MONTHLST         GET A MATCH ON PLACE IN LIST                 
         LA    R4,1                                                             
         SPACE 2                                                                
AN20     CLC   TRHSYEAR(2),0(R3)                                                
         BE    AN22                                                             
         LA    R3,2(,R3)                                                        
         LA    R4,1(,R4)                                                        
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
AN24     GOTO1 PROLLER,DMCB,3,ACCUMS,DUB+2,1,(R4)                               
         B     ANEXT                                                            
         EJECT ,                                                                
***********************************************************************         
*  CLEARING DETAIL LINE                                                         
***********************************************************************         
         SPACE 1                                                                
AN26     L     RE,AMONACC                                                       
         TM    ACMINDS-ACMD(RE),ACMIEMUD+ACMINEWO                               
         BNO   AN26A                                                            
         CLI   MODE,ACCFRST                                                     
         BE    AN26A                                                            
         CLI   QSEQ,QSEQOFF                                                     
         BNE   ANEXT                                                            
*                                                                               
AN26A    GOTO1 PROLLER,DMCB,2,ACCUMS,1  SUBACCT FIRST / ACCFRST                 
         L     R2,ADSUBAC                                                       
*                                                                               
         USING TRSUBHD,R2                                                       
*                                                                               
         CLI   0(R2),X'43'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TRSBACNT,C'*'                                                    
         BNE   *+14                NORMAL SUB-ACC'T.                            
         MVC   P+1(12),TRSBACNT+1  PECULIAR SUB-ACC'T.                          
         B     *+10                                                             
         MVC   P+1(12),TRSBACNT+3                                               
         MVC   THISSUB,TRSBACNT                                                 
         MVC   SAVESUB,SPACES                                                   
         MVC   SAVESUB(12),P+1                                                  
         MVC   SAVEUL,TRSBACNT+1                                                
         SR    R4,R4                                                            
         IC    R4,TRSBLEN                                                       
         SH    R4,=H'18'                                                        
         BM    *+8                                                              
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   SAVESUB+12(0),TRSBNAME                                           
         CLI   QOPT2,C' '          OPTION TO SUB-TOTAL ON . . .                 
         BE    ANEXT               A CHANGE IN CONTRA-ACCOUNT                   
         OC    PREVSUB,PREVSUB                                                  
         BZ    AN28                                                             
         PACK  DUB,QOPT2           QOPT2 INDICATES NUMBER OF BYTES IN           
         CVB   R4,DUB              ACCOUNT CODE FOR COMPARE                     
         LA    R4,2(,R4)           (+3-1)                                       
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   PREVSUB(0),THISSUB                                               
         BE    ANEXT                                                            
         LA    R3,10                                                            
         MVC   P,SPACES                                                         
         BAS   RE,FORMAT                                                        
         CLC   P+32(78),SPACES                                                  
         BE    AN28                                                             
         MVC   P+5(15),=C'TOTALS FOR C/A '                                      
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+20(0),PREVSUB+3                                                
         CP    SUBCOUNT,=P'1'                                                   
         BL    AN28                                                             
         BH    *+14                MORE THAN 1 DETAIL LINE, NORMAL              
         MVC   P,SPACES            1 ONLY, JUST SKIP A LINE.                    
         B     *+8                                                              
         BAS   RE,PRNTIT                                                        
         BAS   RE,PRNTIT                                                        
         SPACE 2                                                                
AN28     MVC   PREVSUB,THISSUB                                                  
         MVC   P,SPACES                                                         
         ZAP   SUBCOUNT,=P'0'                                                   
         GOTO1 PROLLER,DMCB,2,ACCUMS,10                                         
         MVC   P+1(12),SAVESUB                                                  
         B     ANEXT                                                            
         EJECT ,                                                                
***********************************************************************         
*  OFFIRST - FIRST FOR OFFICE  (OFFICE SEQUENCE)                                
***********************************************************************         
         SPACE 1                                                                
         USING ACMD,RF                                                          
         SPACE 1                                                                
AN25     CLI   QSEQ,QSEQOFF                                                     
         BNE   AN25X                                                            
*                                                                               
         L     RF,AMONACC                                                       
         MVC   OFFNAME,ACMOFNAM    EXTRACT OFFICE NAME                          
         L     R2,ACMAOFA                                                       
*                                                                               
         USING OFARECD,R2                                                       
*                                                                               
         MVC   OFFCODE,OFAKOFF     EXTRACT OFFICE CODE                          
*                                                                               
         MVI   FORCEHED,C'Y'       BREAK A PAGE                                 
*                                                                               
         GOTO1 PROLLER,DMCB,2,ACCUMS,7  CLEAR LINE 7                            
*                                                                               
AN25X    B     ANEXT                                                            
*                                                                               
         DROP  R2,RF                                                            
         EJECT ,                                                                
***********************************************************************         
*  OFACFRST - FIRST FOR OFFICE/ACCOUNT (DETAIL SEQUENCE)                        
***********************************************************************         
         SPACE 1                                                                
         USING ACMD,RF                                                          
         USING BUCKBLKD,R2                                                      
         SPACE 1                                                                
AN27     L     RF,AMONACC                                                       
         L     R2,ACMABUCK                                                      
         MVC   BUCKFILT,QOPT3                                                   
         B     ANEXT                                                            
*                                                                               
         DROP  R2,RF                                                            
         EJECT ,                                                                
***********************************************************************         
*  PROCCBUK - PROCESS AN ACCOUNT/OFFICE/CAC BUCKET                              
***********************************************************************         
         SPACE 1                                                                
AN29     BAS   RE,CLIFILT                                                       
         CLI   WANT,C'Y'           TEST WANT THIS CAC                           
         BNE   *+8                 NO                                           
         BAS   RE,BUCKET           YES-HANDLE BUCKET RECORD                     
         B     ANEXT                                                            
         SPACE 2                                                                
*              SPECIAL LAST TIME ROUTINES                                       
         SPACE 3                                                                
AN30     DS    0H                  SUB-ACCOUNT LAST                             
*                                                                               
         L     RE,AMONACC                                                       
         TM    ACMINDS-ACMD(RE),ACMIEMUD+ACMINEWO                               
         BNO   *+12                                                             
         CLI   QSEQ,QSEQOFF                                                     
         BNE   ANEXT                                                            
*                                                                               
         GOTO1 PROLLER,DMCB,6,ACCUMS                                            
         BAS   RE,BLOCPOST                                                      
         CLI   QOPT2,C' '                                                       
         BE    AN31                                                             
         MVC   BLOCSAVE,SAVESUB                                                 
         MVC   SAVESUB(48),SPACES                                               
         MVC   SAVESUB(12),=12X'FF'                                             
         PACK  DUB,QOPT2                                                        
         CVB   R5,DUB                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   SAVESUB(0),BLOCSAVE                                              
         MVC   SAVESUB+12(17),=C'* C/A SUB-TOTAL *  '                           
         BAS   RE,BLOCPOST                                                      
         MVC   SAVESUB(48),BLOCSAVE                                             
         SPACE 2                                                                
AN31     BAS   RE,FORMAT                                                        
         CLC   P+32(78),SPACES                                                  
         BNE   *+14                                                             
         CLC   PSECOND+32(78),SPACES                                            
         BE    ANEXT                                                            
         AP    SUBCOUNT,=P'1'                                                   
         MVC   WORK(36),SAVESUB+12                                              
         GOTO1 PROLLER,DMCB,2,ACCUMS,1                                          
         SPACE 2                                                                
         MVC   CHUNK,SPACES                                                     
         MVC   CHUNK(15),P+1                                                    
         MVC   CHUNK+20(36),WORK                                                
         OC    CHUNK,SPACES                                                     
         CLC   CHUNK,SPACES                                                     
         BE    AN3601                                                           
         GOTO1 ADSQUASH,DMCB,CHUNK,60                                           
         XC    DMCB,DMCB                                                        
         GOTO1 CHOPPER,DMCB,(60,CHUNK),(31,P+1),(C'P',2)                        
AN3601   CLI   QOPT4,C'Y'                                                       
         BNE   AN37                                                             
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         B     ANEXT                                                            
AN37     DS    0H                                                               
         BAS   RE,PRNTIT                                                        
         MVI   SWITCH,C'Y'                                                      
         B     ANEXT                                                            
         EJECT ,                                                                
***********************************************************************         
*  ACCLAST                                                                      
***********************************************************************         
         SPACE 1                                                                
AN38     L     RE,AMONACC                                                       
         TM    ACMINDS-ACMD(RE),ACMIEMUD+ACMINEWO                               
         BNO   AN38X                                                            
         CLI   QSEQ,QSEQDET                                                     
         BNE   AN38N                                                            
         CLI   QOPT4,C'Y'          PRINT SUMMARY ONLY                           
         BE    ANEXT                                                            
         GOTO1 PROLLER,DMCB,6,ACCUMS                                            
         GOTO1 PROLLER,DMCB,2,ACCUMS,1                                          
         ZAP   OFFCOUNT,=P'0'                                                   
         XC    BUFKEY(BUFKEYLQ),BUFKEY                                          
         L     R2,ADACC                                                         
         MVC   BUFACCT,3(R2)       ACCOUNT CODE                                 
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFREC,1                            
         SPACE 1                                                                
AN38B    TM    DMCB+8,X'80'                                                     
         BO    ANEXT               NO MORE BUFFALO RECORDS                      
         CLC   BUFACCT,3(R2)       ANYTHING FOR THIS ACCOUNT?                   
         BNE   ANEXT               NO                                           
         CLC   BUFCULC,=14X'FF'    ANY MORE CONTRA-ACCOUNTS?                    
         BE    AN38J               NO                                           
         CLC   BUFOFFC,=X'FFFF'    IS THIS THE CONTRA-ACCOUNT TOTAL?            
         BE    AN38D                                                            
         CP    OFFCOUNT,=P'1'      HAVE WE PRINTED ANY OFFICES YET?             
         BNL   AN38C                                                            
         MVC   CHUNK,SPACES                                                     
         MVC   CHUNK(12),BUFCULC+2 CONTRA-ACCOUNT CODE                          
         MVC   CHUNK+20(36),BUFCOM CONTRA-ACCOUNT NAME                          
         OC    CHUNK,SPACES                                                     
         GOTO1 ADSQUASH,DMCB,CHUNK,60                                           
         XC    DMCB,DMCB                                                        
         GOTO1 CHOPPER,DMCB,(60,CHUNK),(31,P+1),(C'P',2)                        
         BAS   RE,PRNTIT                                                        
*                                                                               
AN38C    BAS   RE,FORMATB                                                       
         MVC   P+3(2),BUFOFFC      OFFICE CODE                                  
         L     R3,AMONACC                                                       
*                                                                               
         USING ACMD,R3                                                          
*                                                                               
         L     RE,ACMNOFNB                                                      
         ST    RE,8(,R1)                                                        
         GOTO1 BINSRCH,DMCB,BUFOFFC,ACMAOFNB,,ACMOFNL,L'BUFOFFC                 
*                                                                               
         DROP  R3                                                               
*                                                                               
         TM    0(R1),X'01'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RF,0(,R1)                                                        
         MVC   WORK,2(RF)                                                       
         GOTO1 CHOPPER,DMCB,(36,WORK),(27,P+6),(C'P',2)                         
         BAS   RE,PRNTIT                                                        
         AP    OFFCOUNT,=P'1'                                                   
         B     AN38G                                                            
*                                                                               
AN38D    CP    OFFCOUNT,=P'1'      HOW MANY OFFICES PRINTED?                    
         BNL   *+6                                                              
         DC    H'0'                MUST HAVE BEEN AT LEAST ONE OFFICE           
         BE    AN38F               ONLY ONE, DON'T PRINT TOTAL LINE             
         MVC   P+3(7),=C'*TOTAL*'                                               
         BAS   RE,FORMATB                                                       
         BAS   RE,PRNTIT                                                        
*                                                                               
AN38F    ZAP   OFFCOUNT,=P'0'                                                   
AN38G    GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFREC,1                             
         B     AN38B                                                            
*                                                                               
AN38J    BAS   RE,PRNTIT           SKIP A LINE                                  
         MVC   P+1(13),=C'OFFICE TOTALS'                                        
         BAS   RE,PRNTIT                                                        
AN38L    BAS   RE,FORMATB                                                       
         MVC   P+3(2),BUFOFFC      OFFICE CODE                                  
         L     R3,AMONACC                                                       
*                                                                               
         USING ACMD,R3                                                          
*                                                                               
         L     RE,ACMNOFNB                                                      
         ST    RE,8(,R1)                                                        
         GOTO1 BINSRCH,DMCB,BUFOFFC,ACMAOFNB,,ACMOFNL,L'BUFOFFC                 
*                                                                               
         DROP  R3                                                               
*                                                                               
         TM    0(R1),X'01'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RF,0(,R1)                                                        
         MVC   WORK,2(RF)                                                       
         GOTO1 CHOPPER,DMCB,(36,WORK),(27,P+6),(C'P',2)                         
         BAS   RE,PRNTIT                                                        
         GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFREC,1                             
         CLC   BUFACCT,3(R2)       ANY MORE OFFICES?                            
         BE    AN38L               YES                                          
         LA    R3,3                                                             
         B     AN40                PRINT LEVELLED SUB-TOTALS                    
         EJECT ,                                                                
         SPACE 1                                                                
AN38N    CLI   QSEQ,QSEQDET                                                     
         BE    AN38X                                                            
         CLI   QSEQ,QSEQOFF                                                     
         BE    AN38X                                                            
         CLI   QOPT4,C'Y'          PRINT SUMMARY ONLY                           
         BE    ANEXT                                                            
         GOTO1 PROLLER,DMCB,6,ACCUMS                                            
         GOTO1 PROLLER,DMCB,2,ACCUMS,1                                          
         MVI   MYFULL,0            NO ACTIVITY FOR THIS ACCOUNT YET             
         XC    BUFKEY(BUFKEYLQ),BUFKEY                                          
         L     R2,ADACC                                                         
         MVC   BUFACCT,3(R2)       ACCOUNT CODE                                 
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFREC,1                            
*                                                                               
AN38O    TM    DMCB+8,X'80'                                                     
         BO    AN38P               NO MORE BUFFALO RECORDS                      
         CLC   BUFACCT,3(R2)       ANYTHING FOR THIS ACCOUNT?                   
         BNE   AN38P               NO                                           
         BAS   RE,FORMATB                                                       
         MVC   CHUNK,SPACES                                                     
         MVC   CHUNK(12),BUFCULC+2 CONTRA-ACCOUNT CODE                          
         MVC   CHUNK+20(36),BUFCOM CONTRA-ACCOUNT NAME                          
         OC    CHUNK,SPACES                                                     
         GOTO1 ADSQUASH,DMCB,CHUNK,60                                           
         XC    DMCB,DMCB                                                        
         GOTO1 CHOPPER,DMCB,(60,CHUNK),(31,P+1),(C'P',2)                        
         BAS   RE,PRNTIT                                                        
         MVI   MYFULL,X'FF'        WE'VE PRINTED FOR THIS ACCOUNT               
         GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFREC,1                             
         B     AN38O                                                            
AN38P    CLI   MYFULL,X'FF'                                                     
         BNE   *+8                                                              
         BAS   RE,PRNTIT           SKIP LINE BEFORE SUB-TOTALS                  
         LA    R3,3                                                             
         B     AN40                PRINT LEVELLED SUB-TOTALS                    
         EJECT ,                                                                
         SPACE 1                                                                
AN38X    CLI   QOPT2,C' '          ACCLAST                                      
         BE    AN40                                                             
         MVC   P,SPACES                                                         
         LA    R3,10                                                            
         BAS   RE,FORMAT                                                        
         CP    SUBCOUNT,=P'2'                                                   
         BL    AN38Y                                                            
         MVC   P+5(15),=C'TOTALS FOR C/A '                                      
         PACK  DUB,QOPT2                                                        
         CVB   R4,DUB                                                           
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+20(0),PREVSUB+3                                                
         BAS   RE,PRNTIT                                                        
         SPACE 2                                                                
AN38Y    XC    PREVSUB,PREVSUB                                                  
         MVC   P,SPACES                                                         
         ZAP   SUBCOUNT,=P'0'                                                   
         GOTO1 PROLLER,DMCB,2,ACCUMS,10                                         
         LA    R3,3                                                             
         B     AN40                                                             
         EJECT ,                                                                
***********************************************************************         
*  OFFLAST ROUTINE - OFFICE SEQUENCE                                            
***********************************************************************         
         SPACE 1                                                                
AN39     CLI   QSEQ,QSEQOFF        TEST OFFICE SEQUENCE                         
         BNE   AN39X                                                            
*                                                                               
         MVC   OFFCODE,SPACES      CLEAR OFFICE CODE/NAME                       
         MVC   OFFNAME,SPACES                                                   
*                                                                               
AN39X    B     ANEXT                                                            
         EJECT ,                                                                
***********************************************************************         
*  GENERAL LAST TIME ROUTINES                                                   
***********************************************************************         
         SPACE 1                                                                
         USING ACHEIRD,R2                                                       
         SPACE 1                                                                
AN40     L     R2,ADLDGHIR                                                      
         MVC   P,SPACES                                                         
         CLI   QOPT4,C'Y'                                                       
         BE    AN46                                                             
         GOTO1 PROLLER,DMCB,1,ACCUMS,(R3)                                       
         L     R1,DMCB                                                          
         CLC   0(78,R1),=13PL6'0'                                               
         BE    AN46                                                             
         MVI   FORCEHED,C'N'                                                    
         MVC   TOTAB+15(15),ACHRDESC                                            
         MVC   TOTAB+30(15),ACHRDESB                                            
         MVC   TOTAB+45(15),ACHRDESA                                            
         CLI   SWITCH,C'Y'                                                      
         BNE   *+12                                                             
         MVI   SWITCH,C'N'                                                      
         BAS   RE,PRNTIT                                                        
         SPACE 2                                                                
         BAS   RE,FORMAT                                                        
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
         BNE   *+14                                                             
         MVC   HEAD7,SPACES                                                     
         B     AN45                                                             
         CLI   MODE,LEVALAST                                                    
         BNE   AN45A                                                            
         CLI   PROGPROF,C'Y'                                                    
         BNE   AN45A                                                            
AN45     MVI   FORCEHED,C'Y'                                                    
AN45A    MVI   SPACING,2                                                        
         MVI   RCSUBPRG,1                                                       
         BAS   RE,PRNTIT                                                        
         MVI   RCSUBPRG,0                                                       
AN46     CLI   MODE,REQLAST                                                     
         BNE   ANEXT                                                            
         MVI   RCSUBPRG,1                                                       
         MVC   SAVACC,SPACES                                                    
*                                                                               
         L     RE,AMONACC                                                       
         TM    ACMINDS-ACMD(RE),ACMIEMUD+ACMINEWO                               
         BO    *+16                                                             
         BAS   RE,BLOCPRNT                                                      
         MVI   RCSUBPRG,0                                                       
         B     ANEXT                                                            
*                                                                               
         CLI   QSEQ,QSEQDET                                                     
         BNE   *+16                                                             
         BAS   RE,SUMMDET                                                       
         BAS   RE,RECAP                                                         
         B     ANEXT                                                            
*                                                                               
         CLI   QSEQ,QSEQOFF                                                     
         BNE   *+16                                                             
         BAS   RE,SUMMOFF                                                       
         BAS   RE,RECAP                                                         
         B     ANEXT                                                            
*                                                                               
         BAS   RE,SUMMACC          MUST BE ACCOUNT SEQUENCE                     
         B     ANEXT                                                            
         SPACE 2                                                                
TOTAB    DC    CL15'ACCOUNT'                                                    
         DC    CL45' '                                                          
         DC    CL15'LEDGER'                                                     
         DC    CL15'UNIT'                                                       
         DC    CL15'REQUEST'                                                    
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO POST INTO ACCUMULATOR BLOCK                                       
***********************************************************************         
         SPACE 1                                                                
BLOCPOST NTR1                                                                   
         GOTO1 PROLLER,DMCB,1,ACCUMS,2                                          
         CLI   QSEQ,QSEQOFF                                                     
         BE    BLEXT                                                            
         L     R4,DMCB                                                          
         CLC   0(78,R4),=13PL6'0'                                               
         BE    BLEXT                                                            
         XC    BUFREC(BUFRECLQ),BUFREC                                          
         MVC   BUFCULC,SAVEUL                                                   
         MVC   BUFCOM,SAVEUL+14                                                 
         LA    R3,13                                                            
         LA    R2,BUFACCS                                                       
         SPACE 2                                                                
BL2      ZAP   0(8,R2),0(6,R4)                                                  
         LA    R4,6(,R4)                                                        
         LA    R2,8(,R2)                                                        
         BCT   R3,BL2                                                           
         SPACE 2                                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         SPACE 2                                                                
BLEXT    XIT1                                                                   
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
         LA    R5,BUFACCS          R5=A(OFFICE BUCKETS)                         
         LA    R1,12               R1=LOOP COUNTER                              
         MVC   MYFULL,=F'1'        COLUMN NUMBER                                
*                                                                               
BUCKET5  CLC   BUKMOS,0(R2)        MATCH ON MOS                                 
         BE    BUCKET6             YES                                          
         LA    R2,2(,R2)                                                        
         LA    R5,L'BUFACCS(,R5)                                                
         L     RE,MYFULL                                                        
         LA    RE,1(,RE)                                                        
         ST    RE,MYFULL                                                        
         BCT   R1,BUCKET5                                                       
         B     BUCKET3             NOT IN REPORT PERIOD-NEXT ELEM               
*                                                                               
BUCKET6  ZAP   DUB,BUKDR                                                        
         CLI   QOPT1,C'D'          TEST REPORTING DEBITS                        
         BE    BUCKET7             YES                                          
         SP    DUB,BUKCR                                                        
         CLI   QOPT1,C'B'          TEST REPORTING BALANCES                      
         BE    BUCKET7             YES                                          
         ZAP   DUB,BUKCR           NO-SHOWING CREDITS                           
*                                                                               
BUCKET7  XC    BUFREC(BUFRECLQ),BUFREC                                          
         L     RF,ADSUBAC                                                       
*                                                                               
         USING TRSUBHD,RF                                                       
*                                                                               
         CLI   0(RF),X'43'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         BM    *+8                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BUFCOM(0),TRSBNAME                                               
*                                                                               
         DROP  RF                                                               
*                                                                               
         CLI   QSEQ,QSEQDET                                                     
         BE    BUCKET8                                                          
         CLI   QSEQ,QSEQOFF                                                     
         BE    BUCKET9                                                          
*                                                                               
         L     RF,ACMABUK                                                       
*                                                                               
         USING CACRECD,RF                                                       
*                                                                               
         MVC   BUFACCT,CACKACT     ACCOUNT CODE                                 
         MVC   BUFCULC,CACKULC     CONTRA-ACCOUNT CODE                          
*                                                                               
         DROP  RF                                                               
*                                                                               
         LA    R0,13                                                            
         LA    RF,BUFACCS                                                       
         ZAP   0(L'BUFACCS,RF),=P'0'                                            
         LA    RF,L'BUFACCS(,RF)                                                
         BCT   R0,*-10                                                          
         ZAP   0(L'BUFACCS,R5),DUB UPDATE MONTH ACCUMULATOR                     
         ZAP   BUFTOTAL,DUB        UPDATE TOTAL ACCUMULATOR                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         MVC   BUFACCT,=12X'FF'    CONTRA TOTAL ACROSS ACCOUNTS                 
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         B     BUCKET10                                                         
*                                                                               
         USING CACRECD,RF                                                       
*                                                                               
BUCKET8  L     RF,ACMABUK                                                       
         MVC   BUFACCT,CACKACT     ACCOUNT CODE                                 
         MVC   BUFCULC,CACKULC     CONTRA-ACCOUNT CODE                          
         MVC   BUFOFFC,CACKOFF     OFFICE CODE                                  
*                                                                               
         DROP  RF                                                               
*                                                                               
         LA    R0,13                                                            
         LA    RF,BUFACCS                                                       
         ZAP   0(L'BUFACCS,RF),=P'0'                                            
         LA    RF,L'BUFACCS(,RF)                                                
         BCT   R0,*-10                                                          
         ZAP   0(L'BUFACCS,R5),DUB UPDATE MONTH ACCUMULATOR                     
         ZAP   BUFTOTAL,DUB        UPDATE TOTAL ACCUMULATOR                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         MVC   BUFOFFC,=X'FFFF'    CONTRA TOTAL ACROSS OFFICES                  
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
*                                                                               
         MVC   BUFCULC,=14X'FF'    OFFICE TOTAL ACROSS CONTRAS                  
         L     R5,ACMABUK                                                       
*                                                                               
         USING CACRECD,R5                                                       
*                                                                               
         MVC   BUFOFFC,CACKOFF                                                  
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         MVC   BUFACCT,=12X'FF'    CONTRA TOTAL ACROSS ACCOUNTS                 
         MVC   BUFCULC,CACKULC                                                  
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         MVC   BUFOFFC,=X'FFFF'    CONTRA TOTAL ACROSS OFFICE/ACCOUNTS          
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
*                                                                               
         MVC   BUFOFFC,CACKOFF                                                  
*                                                                               
         DROP  R5                                                               
*                                                                               
         MVC   BUFACCT,=12X'FF'    GRAND TOTAL FOR AN OFFICE                    
         MVC   BUFCULC,=14X'FF'                                                 
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         MVC   BUFOFFC,=X'FFFF'    GRAND TOTAL FOR THE ENTIRE REPORT            
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         B     BUCKET10                                                         
*                                                                               
         USING CACRECD,RF                                                       
*                                                                               
BUCKET9  L     RF,ACMABUK                                                       
         MVC   BUFOFFCO,CACKOFF    OFFICE CODE                                  
         MVC   BUFCULCO,CACKULC    CONTRA-ACCOUNT CODE                          
*                                                                               
         DROP  RF                                                               
*                                                                               
         LA    R0,13                                                            
         LA    RF,BUFACCS                                                       
         ZAP   0(L'BUFACCS,RF),=P'0'                                            
         LA    RF,L'BUFACCS(,RF)                                                
         BCT   R0,*-10                                                          
         ZAP   0(L'BUFACCS,R5),DUB UPDATE MONTH ACCUMULATOR                     
         ZAP   BUFTOTAL,DUB        UPDATE TOTAL ACCUMULATOR                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         MVC   BUFCULCO,=14X'FF'                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         MVC   BUFACCT,=12X'FF'    OFFICE RECAP RECORD                          
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         MVC   BUFOFFCO,=X'FFFF'   GRAND TOTAL FOR THE ENTIRE REPORT            
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
*                                                                               
BUCKET10 L     R2,MYFULL           SHIFT COLUMN NUMBER                          
         GOTO1 PROLLER,DMCB,3,ACCUMS,DUB+2,1,(R2)                               
         B     BUCKET3             NEXT ELEMENT                                 
*                                                                               
BUCKETX  B     BLEXT                                                            
         DROP  R3,R4                                                            
         EJECT ,                                                                
***********************************************************************         
*  PRINT ROUTINE                                                                
***********************************************************************         
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         CLI   RCSUBPRG,2          OFFICE RECAP?                                
         BE    PRNTIT4             YES                                          
*                                                                               
         CLI   SUMMFLAG,X'FF'                                                   
         BNE   *+10                                                             
         MVC   HEAD4+49(15),=C'REQUEST SUMMARY'                                 
         CLI   QSEQ,QSEQDET                                                     
         BNE   PRNTIT2                                                          
         MVC   HEAD4+47(18),=C'WITH OFFICE DETAIL'                              
         CLI   SUMMFLAG,X'FF'                                                   
         BNE   *+10                                                             
         MVC   HEAD4+39(34),=C'REQUEST SUMMARY WITH OFFICE DETAIL'              
         B     PRNTIT4                                                          
*                                                                               
PRNTIT2  CLI   QSEQ,QSEQOFF                                                     
         BNE   PRNTIT4                                                          
         MVC   HEAD7+85(2),OFFCODE                                              
         MVC   HEAD7+88(22),OFFNAME                                             
         MVC   HEAD4+49(15),=C'   BY OFFICE   '                                 
         CLI   SUMMFLAG,X'FF'                                                   
         BNE   *+10                                                             
         MVC   HEAD4+44(25),=C'REQUEST SUMMARY BY OFFICE'                       
*                                                                               
PRNTIT4  MVC   HEAD6+85(17),OPTLN                                               
         MVC   HEAD7+1(25),SAVACC                                               
         MVC   HEAD10+37(66),DATELN                                             
         MVC   HEAD11+37(66),DATELN+66                                          
*                                                                               
         GOTO1 ACREPORT                                                         
         B     BLEXT                                                            
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO PRINT OUT A SUMMARY BLOCK                                         
***********************************************************************         
         SPACE 1                                                                
BLOCPRNT NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVI   SUMMFLAG,X'FF'                                                   
         XC    BUFKEY(BUFKEYLQ),BUFKEY                                          
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFREC,1                            
         SPACE 1                                                                
BL10     TM    DMCB+8,X'80'                                                     
         BO    BL20                                                             
         LA    R2,BUFREC                                                        
         GOTO1 ADSQUASH,DMCB,(R2),64                                            
         XC    DMCB,DMCB                                                        
         GOTO1 CHOPPER,DMCB,(64,(R2)),(31,P+1),(C'P',2)                         
         BAS   RE,FORMATB                                                       
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFREC,1                             
         B     BL10                                                             
         SPACE 1                                                                
BL20     GOTO1 BUFFALO,DMCB,=C'RESET',ADBUFC                                    
         B     BLEXT                                                            
         EJECT ,                                                                
***********************************************************************         
*  FORMAT AND CLEAR A LINE OF ACCUMULATORS                                      
***********************************************************************         
         SPACE 1                                                                
FORMAT   NTR1                                                                   
         GOTO1 PROLLER,DMCB,1,ACCUMS,(R3)                                       
         L     R2,DMCB                                                          
         MVC   CHUNK,SPACES                                                     
         LA    R3,CHUNK                                                         
         MVC   CHUNK2,SPACES                                                    
         LA    R4,13                                                            
         SPACE 2                                                                
FORMAT2  CH    R4,=H'1'                                                         
         BNE   *+8                                                              
         LA    R3,P+99                                                          
         CP    0(6,R2),=P'0'                                                    
         BE    FORMAT4                                                          
         CLI   QOPT5,C'Y'          OPTION FOR BIG NUMBERS                       
         BE    FORMAT2A                                                         
         EDIT  (P6,(R2)),(11,0(R3)),2,FLOAT=-                                   
         B     FORMAT3                                                          
         SPACE 1                                                                
FORMAT2A ZAP   DIV,0(6,R2)         ROUND,DROP PENNIES AND EDIT                  
         AP    DIV,=P'50'                                                       
         CP    DIV,=P'0'                                                        
         BNL   *+10                                                             
         SP    DIV,=P'100'                                                      
         DP    DIV,=P'100'                                                      
         EDIT  (P8,DIV),(11,0(R3)),FLOAT=-                                      
         SPACE 2                                                                
FORMAT3  ZAP   0(6,R2),=P'0'                                                    
         SPACE 2                                                                
FORMAT4  LA    R2,6(,R2)                                                        
         LA    R3,11(,R3)                                                       
         BCT   R4,FORMAT2                                                       
         MVC   P+32(66),CHUNK                                                   
         MVC   PSECOND+32(66),CHUNK+66                                          
         CLC   PSECOND,SPACES                                                   
         BE    *+8                                                              
         MVI   SPACING,2                                                        
         B     BLEXT                                                            
         EJECT ,                                                                
***********************************************************************         
*  FORMAT 13X8-BYTE ACCUMULATORS                                                
***********************************************************************         
         SPACE 1                                                                
FORMATB  NTR1                                                                   
         LA    R2,BUFACCS                                                       
         LA    R4,13                                                            
         MVC   CHUNK,SPACES                                                     
         MVC   CHUNK2,SPACES                                                    
         LA    R3,CHUNK                                                         
         SPACE 2                                                                
FORMB2   CH    R4,=H'1'                                                         
         BNE   *+8                                                              
         LA    R3,P+99                                                          
         CP    0(8,R2),=P'0'                                                    
         BE    FORMB4                                                           
         EDIT  (P8,(R2)),(11,0(R3)),2,FLOAT=-                                   
         ZAP   0(8,R2),=P'0'                                                    
         SPACE 1                                                                
FORMB4   LA    R2,8(,R2)                                                        
         LA    R3,11(,R3)                                                       
         BCT   R4,FORMB2                                                        
         MVC   P+32(66),CHUNK                                                   
         MVC   PSECOND+32(66),CHUNK+66                                          
         CLC   PSECOND,SPACES                                                   
         BE    *+8                                                              
         MVI   SPACING,2                                                        
         B     BLEXT                                                            
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
         BE    BLEXT               NO FILTERS                                   
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
         BNE   BLEXT               DON'T EXCLUDE THIS CLIENT                    
         MVI   WANT,C'N'           EXCLUDE CLIENT                               
         B     BLEXT                                                            
         SPACE 1                                                                
CLIFILT5 CLC   THREE,QSELECT                                                    
         BE    BLEXT               WANT THIS CLIENT                             
         TM    QSELECT,X'40'       'NOT' LOGIC                                  
         BO    CLIFILTN                                                         
         NI    THREE,X'BF'                                                      
         CLC   THREE,QSELECT       CAN WE EXCLUDE THIS CLIENT                   
         BNE   *+8                                                              
CLIFILTN MVI   WANT,C'N'           IF EQUAL, EXCLUDE THIS CLIENT                
         OI    THREE,X'40'                                                      
         B     BLEXT                                                            
         EJECT ,                                                                
         SPACE 1                                                                
SUMMACC  NTR1                                                                   
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   SUMMFLAG,X'FF'                                                   
         XC    BUFKEY(BUFKEYLQ),BUFKEY                                          
         MVC   BUFACCT,=12X'FF'    SUMMARY RECORDS                              
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFREC,1                            
*                                                                               
SUMM1    TM    DMCB+8,X'80'                                                     
         BO    SUMM1X              NO MORE BUFFALO RECORDS                      
         MVC   CHUNK,SPACES                                                     
         MVC   CHUNK(14),BUFCULC   UNIT/LEDGER/CONTRA-ACCOUNT CODE              
         MVC   CHUNK+20(36),BUFCOM CONTRA-ACCOUNT NAME                          
         OC    CHUNK,SPACES                                                     
         GOTO1 ADSQUASH,DMCB,CHUNK,60                                           
         XC    DMCB,DMCB                                                        
         GOTO1 CHOPPER,DMCB,(60,CHUNK),(31,P+1),(C'P',2)                        
         BAS   RE,FORMATB                                                       
         GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFREC,1                             
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         B     SUMM1                                                            
*                                                                               
SUMM1X   B     BLEXT                                                            
         EJECT ,                                                                
         SPACE 1                                                                
SUMMDET  NTR1                                                                   
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   SUMMFLAG,X'FF'                                                   
         ZAP   OFFCOUNT,=P'0'                                                   
         XC    BUFKEY(BUFKEYLQ),BUFKEY                                          
         MVC   BUFACCT,=12X'FF'    SUMMARY RECORDS                              
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFREC,1                            
*                                                                               
SUMMB    TM    DMCB+8,X'80'                                                     
         BO    SUMMX               NO MORE BUFFALO RECORDS                      
         CLC   BUFCULC,=14X'FF'    ANY MORE CONTRA-ACCOUNTS?                    
         BE    SUMMX               NO                                           
         CLC   BUFOFFC,=X'FFFF'    TOTAL FOR A CONTRA-ACCOUNT?                  
         BE    SUMMD               NO                                           
         CP    OFFCOUNT,=P'1'      HAVE WE PRINTED ANY OFFICES YET?             
         BNL   SUMMC                                                            
         BAS   RE,PRNTIT           SKIP A LINE                                  
         MVC   CHUNK,SPACES                                                     
         MVC   CHUNK(14),BUFCULC   UNIT/LEDGER/CONTRA-ACCOUNT CODE              
         MVC   CHUNK+20(36),BUFCOM CONTRA-ACCOUNT NAME                          
         OC    CHUNK,SPACES                                                     
         GOTO1 ADSQUASH,DMCB,CHUNK,60                                           
         XC    DMCB,DMCB                                                        
         GOTO1 CHOPPER,DMCB,(60,CHUNK),(31,P+1),(C'P',2)                        
         BAS   RE,PRNTIT                                                        
*                                                                               
SUMMC    BAS   RE,FORMATB                                                       
         MVC   P+3(2),BUFOFFC      OFFICE CODE                                  
         L     R3,AMONACC                                                       
*                                                                               
         USING ACMD,R3                                                          
*                                                                               
         L     RE,ACMNOFNB                                                      
         ST    RE,8(,R1)                                                        
         GOTO1 BINSRCH,DMCB,BUFOFFC,ACMAOFNB,,ACMOFNL,L'BUFOFFC                 
*                                                                               
         DROP  R3                                                               
*                                                                               
         TM    0(R1),X'01'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RF,0(,R1)                                                        
         MVC   WORK,2(RF)                                                       
         GOTO1 CHOPPER,DMCB,(36,WORK),(27,P+6),(C'P',2)                         
         BAS   RE,PRNTIT                                                        
         AP    OFFCOUNT,=P'1'                                                   
         B     SUMMG                                                            
*                                                                               
SUMMD    CP    OFFCOUNT,=P'1'      HOW MANY OFFICES PRINTED?                    
         BNL   *+6                                                              
         DC    H'0'                MUST HAVE BEEN AT LEAST ONE OFFICE           
         BE    SUMMF               ONLY ONE, DON'T PRINT TOTAL LINE             
         MVC   P+3(7),=C'*TOTAL*'                                               
         BAS   RE,FORMATB                                                       
         BAS   RE,PRNTIT                                                        
*                                                                               
SUMMF    ZAP   OFFCOUNT,=P'0'                                                   
SUMMG    GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFREC,1                             
         B     SUMMB                                                            
*                                                                               
SUMMX    B     BLEXT                                                            
         EJECT ,                                                                
         SPACE 1                                                                
SUMMOFF  NTR1                                                                   
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   SUMMFLAG,X'FF'                                                   
         ZAP   OFFCOUNT,=P'0'                                                   
         XC    BUFKEY(BUFKEYLQ),BUFKEY                                          
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFREC,1                            
*                                                                               
SUMMOB   TM    DMCB+8,X'80'                                                     
         BO    SUMMOX              NO MORE BUFFALO RECORDS                      
         CLC   BUFACCT,=12X'FF'    ANY MORE SUMMARY RECORDS?                    
         BE    SUMMOX              NO                                           
         CLC   BUFCULCO,=14X'FF'   MORE CONTRA-ACCOUNTS FOR OFFICE?             
         BE    SUMMOD              NO                                           
*                                                                               
         MVC   OFFCODE,BUFOFFCO    OFFICE CODE                                  
         L     R3,AMONACC                                                       
*                                                                               
         USING ACMD,R3                                                          
*                                                                               
         L     RE,ACMNOFNB                                                      
         ST    RE,8(,R1)                                                        
         GOTO1 BINSRCH,DMCB,BUFOFFCO,ACMAOFNB,,ACMOFNL,L'BUFOFFCO               
*                                                                               
         DROP  R3                                                               
*                                                                               
         TM    0(R1),X'01'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RF,0(,R1)                                                        
         MVC   OFFNAME,2(RF)       OFFICE NAME                                  
*                                                                               
         MVC   CHUNK,SPACES                                                     
         MVC   CHUNK(14),BUFCULCO  UNIT/LEDGER/CONTRA-ACCOUNT CODE              
         MVC   CHUNK+20(36),BUFCOM CONTRA-ACCOUNT NAME                          
         OC    CHUNK,SPACES                                                     
         GOTO1 ADSQUASH,DMCB,CHUNK,60                                           
         XC    DMCB,DMCB                                                        
         GOTO1 CHOPPER,DMCB,(60,CHUNK),(31,P+1),(C'P',2)                        
         BAS   RE,FORMATB                                                       
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         B     SUMMOG                                                           
*                                                                               
SUMMOD   MVC   P+3(7),=C'*TOTAL*'                                               
         BAS   RE,FORMATB                                                       
         BAS   RE,PRNTIT                                                        
         MVI   FORCEHED,C'Y'       NEW PAGE ON OFFICE CHANGE                    
*                                                                               
SUMMOG   GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFREC,1                             
         B     SUMMOB                                                           
*                                                                               
SUMMOX   B     BLEXT                                                            
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO PRINT OFFICE RECAP                                                
***********************************************************************         
         SPACE 1                                                                
RECAP    NTR1  ,                                                                
*                                                                               
         MVI   RCSUBPRG,2                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    BUFREC(BUFRECLQ),BUFREC                                          
         MVC   BUFACCT,=12X'FF'    GET OFFICE RECAP RECORDS                     
         CLI   QSEQ,QSEQOFF                                                     
         BE    *+10                                                             
         MVC   BUFCULC,=14X'FF'                                                 
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFREC,1                            
*                                                                               
RECAP4   TM    DMCB+8,X'90'        ANYTHING TO PRINT?                           
         BNZ   RECAPX              NO                                           
         MVC   HALF,BUFOFFC                                                     
         CLI   QSEQ,QSEQOFF                                                     
         BNE   *+10                                                             
         MVC   HALF,BUFOFFCO                                                    
         CLC   HALF,=X'FFFF'       STILL LOOKING AT OFFICE TOTALS?              
         BE    RECAP6              NO                                           
*                                                                               
         MVI   SPACING,2                                                        
         MVC   CHUNK,SPACES                                                     
         MVC   CHUNK(2),HALF       OFFICE CODE                                  
         L     R2,AMONACC                                                       
*                                                                               
         USING ACMD,R2                                                          
*                                                                               
         L     RE,ACMNOFNB                                                      
         ST    RE,8(,R1)                                                        
         GOTO1 BINSRCH,DMCB,HALF,ACMAOFNB,,ACMOFNL,L'BUFOFFC                    
*                                                                               
         DROP  R2                                                               
*                                                                               
         TM    0(R1),X'01'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RF,0(,R1)                                                        
         MVC   CHUNK+4(36),2(RF)                                                
         GOTO1 CHOPPER,DMCB,(40,CHUNK),(31,P+1),(C'P',2)                        
         BAS   RE,FORMATB                                                       
         BAS   RE,PRNTIT                                                        
         GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFREC,1                             
         B     RECAP4                                                           
*                                                                               
RECAP6   MVC   CHUNK,SPACES                                                     
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
         BAS   RE,FORMATB                                                       
         BAS   RE,PRNTIT                                                        
*                                                                               
RECAPX   MVI   RCSUBPRG,0                                                       
         B     BLEXT                                                            
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
*  WORK SPACE ETC., FOR MODULE                                                  
***********************************************************************         
         SPACE 1                                                                
PREVSUB  DC    XL15'00'                                                         
THISSUB  DC    XL15'00'                                                         
BLOCSAVE DS    CL48                                                             
SUBCOUNT DC    PL4'0'                                                           
OFFCOUNT DC    PL4'0'                                                           
SAVEUL   DS    CL2                                                              
SAVESUB  DC    CL50' '                                                          
SAVACC   DC    CL25' '                                                          
         SPACE 1                                                                
RELOTAB  DS    0A                                                               
         DC    V(BUFFALOC)                                                      
         DC    V(ACCEDIT)                                                       
         DC    X'FF'                                                            
         EJECT ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 3                                                                
         DS    0D                                                               
         DC    CL8'*BUFFALO'                                                    
         BUFF  LINES=950,ROWS=1,COLUMNS=13,FLAVOR=PACKED,              X        
               KEYLIST=(28,A),COMMENT=36                                        
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
DIV      DS    PL10                                                             
MONTHLST DS    CL26                12 PACKED YYMM DATES                         
CHUNK    DS    CL132                                                            
CHUNK2   DS    CL132                                                            
*                                                                               
BUFREC   DS    0D                                                               
BUFKEY   DS    0C                                                               
BUFACCT  DS    CL12                ACCOUNT                                      
BUFCULC  DS    CL14                CONTRA UNIT/LEDGER/CONTRA                    
BUFOFFC  DS    CL2                 OFFICE                                       
         ORG   BUFCULC                                                          
BUFOFFCO DS    CL2                 OFFICE (IN OFFICE SEQUENCE)                  
BUFCULCO DS    CL14                CONTRA UNIT/LEDGER/CONTRA ("O" SEQ.)         
BUFKEYLQ EQU   *-BUFKEY                                                         
BUFCOM   DS    CL36                NAME                                         
BUFACCS  DS    0PL8                                                             
BUFACCMN DS    12PL8               MONTH ACCUMULATORS                           
BUFTOTAL DS    PL8                 TOTAL FOR ALL MONTHS                         
BUFRECLQ EQU   *-BUFREC                                                         
*                                                                               
MYFULL   DS    F                                                                
DATELN   DS    CL132               12 CHARACTER MMM YY COLUMNS                  
OPTLN    DS    CL17                                                             
WANT     DS    CL1                                                              
OFFCODE  DS    CL2                                                              
OFFNAME  DS    CL36                                                             
SUMMFLAG DS    C                                                                
         EJECT ,                                                                
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
**PAN#1  DC    CL21'073ACREP8702 05/01/02'                                      
         END                                                                    
