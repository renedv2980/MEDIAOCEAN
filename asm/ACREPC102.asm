*          DATA SET ACREPC102  AT LEVEL 039 AS OF 05/01/02                      
*PHASE ACC102A,+0                                                               
*INCLUDE PERVERT                                                                
*INCLUDE SQUASHER                                                               
*&&UK                                                                           
*INCLUDE TMUNPK                                                                 
*&&                                                                             
         TITLE 'UNDISBURSED ANALYSIS - ACC1'                                    
         PRINT NOGEN                                                            
ACC12    CSECT                                                                  
         NMOD1 0,**ACC1**,R8,RR=R5                                              
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACC1D,RC                                                         
         ST    R5,RELOC                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCACC                                                     
         BE    PRACC                                                            
         CLI   MODE,SBACFRST                                                    
         BE    SBACF                                                            
         CLI   MODE,PROCTRNS                                                    
         BE    PRTRNS                                                           
         CLI   MODE,ACCLAST                                                     
         BE    ACLAST                                                           
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
EXIT     XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
*              RUNFRST                                                *         
***********************************************************************         
*                                                                               
RUNF     DS    0H                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   REMOTOP,C'Y'                                                     
         MVI   RCSUBPRG,4                                                       
         L     R2,REMOTEC                                                       
         USING REMOTED,R2                                                       
         OC    REMOTKEY,REMOTKEY   IF REMOTE                                    
         BNZ   EXIT                THEN NO BOXES                                
         MVI   REMOTOP,C'N'                                                     
         MVI   RCSUBPRG,4                                                       
         L     R2,=A(SAVERC)                                                    
         A     R2,RELOC                                                         
         ST    RC,0(R2)                                                         
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX                                                         
         L     R2,=V(HOOK)                                                      
         A     R2,RELOC                                                         
         ST    R2,HEADHOOK                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              REQUEST FIRST                                          *         
***********************************************************************         
*                                                                               
REQF     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         CLI   REMOTOP,C'Y'                                                     
         BE    *+8                                                              
         MVI   RCSUBPRG,4                                                       
*                                                                               
         MVI   MEDSW,C'N'          FIND IF A MEDIA LEDGER                       
         LA    R2,MEDLEG                                                        
REQF10   CLI   0(R2),X'FF'                                                      
         BE    REQF20                                                           
         CLC   QLEDGER,0(R2)                                                    
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         B     REQF10                                                           
         MVI   MEDSW,C'Y'                                                       
*                                                                               
REQF20   DS    0H                                                               
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         CLI   ACMMEND,X'FF'               NO MOA END  DATE                     
         BE    REQF50                                                           
         MVC   WORK+6(2),ACMMEND           GET LAST DAY OF THE MONTH            
         MVI   WORK+8,X'28'                                                     
         GOTO1 DATCON,DMCB,(1,WORK+6),(0,WORK)                                  
*                                                                               
         GOTO1 ADDAY,DMCB,(C'M',WORK),(X'80',WORK+6),0                          
         MVC   WORK(6),WORK+6                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(2,DISDATE)                                 
*                                                                               
REQF50   CLI   QOPT1,X'40'                                                      
         BNE   REQF70                                                           
*                                                                               
REQF60   MVC   NUMPARTS(6),DEF                                                  
         B     REQF110                                                          
*                                                                               
REQF70   CLI   QOPT1,C'A'                                                       
         BNE   REQF80                                                           
         MVC   NUMPARTS(6),OPTA                                                 
         B     REQF110                                                          
*                                                                               
REQF80   CLI   QOPT1,C'B'                                                       
         BNE   REQF90                                                           
         MVC   NUMPARTS(6),OPTB                                                 
         B     REQF110                                                          
*                                                                               
REQF90   CLI   QOPT1,C'C'                                                       
         BNE   REQF100                                                          
         MVC   NUMPARTS(6),OPTC                                                 
         B     REQF110                                                          
*                                                                               
REQF100  TM    QOPT1,X'F0'                                                      
         BNO   REQF60              USE DEFAULT                                  
         PACK  DUB(8),QOPT1(2)                                                  
         CVB   R1,DUB                                                           
         STC   R1,PART1W                                                        
         MVI   NUMPARTS,X'04'                                                   
         MVC   PART2W(3),PART1W                                                 
*                                                                               
REQF110  LA    R1,ACPART1          CLEAR ALL ACCUMULATORS                       
         LA    R2,6                                                             
         LA    R3,RQACTOT                                                       
         ZAP   0(6,R1),=P'0'                                                    
         BXLE  R1,R2,*-6                                                        
         XC    RQPART1N(12),RQPART1N                                            
         XC    RQACTOTN,RQACTOTN                                                
         XC    RQHDTOTN,RQHDTOTN                                                
         XC    RQAPTOTN,RQAPTOTN                                                
         CLC   QEND,SPACES                                                      
         BNE   REQF120                                                          
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,QEND)                                  
*                                                                               
REQF120  MVC   ENDDT,QEND                                                       
         MVC   ENDDAY,SPACES                                                    
         GOTO1 DATCON,DMCB,(0,QEND),(8,ENDDAY)                                  
         GOTO1 (RF),(R1),(0,ENDDT),(1,PENDDT)                                   
         GOTO1 (RF),(R1),(4,RCDATE),(2,TODAY2)                                  
*                                                                               
         MVC   RQOFFICE,SPACES                                                  
         MVC   RQOFFICE(1),QOPT3                                                
         CLI   QOPT3,C' '                                                       
         BNE   REQF130                                                          
         MVC   RQOFFICE,QTRNSFLT                                                
         CLC   QTRNSFLT,SPACES                                                  
         BNE   REQF130                                                          
         MVC   RQOFFICE,QOFFICE                                                 
         CLC   QOFFICE,SPACES                                                   
         BNE   REQF130                                                          
         MVC   RQOFFICE,SPACES                                                  
         B     EXIT                                                             
*                                                                               
REQF130  MVC   SAVEKEY,KEY         SAVE MONACC'S KEY                            
         MVC   MYKEY,SPACES                                                     
         MVC   SAVEOFF,SPACES                                                   
         L     R2,ADCOMP                                                        
         MVC   MYKEY(1),0(R2)                                                   
         MVC   MYKEY+1(2),=C'2D'                                                
         MVC   MYKEY+3(2),RQOFFICE                                              
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',MYKEY,MYKEY                      
         CLI   DMCB+8,0                                                         
         BE    REQF140                                                          
         TM    DMCB+8,X'10'                                                     
         BNO   *+12                OFFICE RECORD NOT FOUND                      
         MVC   RQOFFICE,SPACES     IGNORE INPUT                                 
         B     REQF160                                                          
         DC    H'0'                                                             
*                                                                               
REQF140  LA    R4,MYKEY                                                         
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   REQF150             NOT FOUND                                    
         MVC   SAVEOFF(2),MYKEY+3                                               
         MVI   SAVEOFF+2,C','                                                   
         USING ACNAMED,R4                                                       
         ZIC   R2,ACNMLEN                                                       
         SH    R2,=H'3'                                                         
         EX    R2,*+8                                                           
         B     REQF160                                                          
         MVC   SAVEOFF+3(0),ACNMNAME                                            
*                                                                               
REQF150  MVC   SAVEOFF(13),=C'OFFICE FILTER'                                    
         MVC   SAVEOFF+14(2),MYKEY+3                                            
*                                                                               
REQF160  MVC   MYKEY,SPACES                                                     
         MVC   MYKEY(L'SAVEKEY),SAVEKEY                                         
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',MYKEY,MYKEY                      
         B     EXIT                                                             
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        PROCACC                                                      *         
***********************************************************************         
*                                                                               
PRACC    DS    0H                                                               
         L     R9,ADACCNAM                                                      
         USING ACNAMED,R9                                                       
         MVC   DETAIL,SPACES                                                    
         MVC   SAVEAC,SPACES                                                    
         L     R2,ADACC                                                         
         MVC   SAVEAC(12),3(R2)                                                 
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAVEAC+14(0),ACNMNAME                                            
         XC    ACPART1N(12),ACPART1N                                            
         XC    ACHDTOTN,ACHDTOTN                                                
         XC    ACAPTOTN,ACAPTOTN                                                
         XC    ACACTOTN,ACACTOTN                                                
         LA    R3,ACPART1          CLEAR AC ACCUMULATORS                        
         LA    R4,6                                                             
         LA    R5,ACOVER                                                        
         ZAP   0(6,R3),=P'0'                                                    
         BXLE  R3,R4,*-6                                                        
         ZAP   ACHDTOT,=P'0'       CLEAR HELD ACCUMULATOR                       
         ZAP   ACAPTOT,=P'0'       CLEAR APPROVED ACCUM                         
         ZAP   ACACTOT,=P'0'                                                    
         ZAP   ACAUTH,=P'0'                                                     
         ZAP   ACUNAUTH,=P'0'                                                   
         ZAP   ITEMCNT,=P'0'                                                    
         ZAP   SVTRNSAM,=P'0'                                                   
         MVI   SVTRNSTA,0                                                       
         MVI   FIRSTSW,0                                                        
         MVI   LASTSW,0                                                         
         MVI   ACTIVE,C'N'         ACCOUNT ACTIVITY                             
         MVI   CACTIVE,C'N'        CONTRA ACTIVITY                              
         MVI   SUMSW,C'N'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         CLI   REMOTOP,C'Y'                                                     
         BE    *+8                                                              
         MVI   RCSUBPRG,4                                                       
         MVI   FCRDTRNS,C'Y'                                                    
         CLI   QOPT4,C'Y'          NEGATIVE BALANCES ONLY                       
         BNE   EXIT                                                             
         L     R2,ADACCBAL                                                      
         USING ACBALD,R2                                                        
         CP    ACBLCR,ACBLDR                                                    
         BL    EXIT                                                             
         MVI   FCRDTRNS,C'N'                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SUBACC FIRST                                                 *         
***********************************************************************         
*                                                                               
SBACF    DS    0H                                                               
         CLI   PROGPROF+2,C'Y'                                                  
         BNE   SBAC10                                                           
         CLI   CACTIVE,C'N'        WAS PREVIOUS CONTRA ACTIVE?                  
         BE    SBAC10                                                           
         MVI   LASTSW,1                                                         
         BAS   RE,MERGINV                                                       
         MVI   LASTSW,0                                                         
         MVI   CACTIVE,C'N'                                                     
*                                                                               
SBAC10   L     R1,ADSUBAC                                                       
         MVC   CNTRAC,3(R1)                                                     
         MVI   FIRSTSW,0                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        PROCTRNS                                                     *         
***********************************************************************         
*                                                                               
PRTRNS   DS    0H                                                               
         L     R9,ADTRANS                                                       
         USING TRANSD,R9                                                        
         CLI   TRNSEL,X'44'                                                     
         BNE   EXIT                                                             
*                                                                               
         USING ACKEYD,RF                                                        
         L     RF,ADTRANS                                                       
         SH    RF,DATADISP                                                      
         TM    TRNSSTAT,X'20'      REVERSAL?                                    
         BZ    *+10                                                             
         XC    ACDTUSED,ACDTUSED                                                
         DROP  RF                                                               
*                                                                               
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         CLI   ACMMEND,X'FF'       IS THERE A MOS FILTER                        
         BNE   PROC30              IF THERE IS CHECK USED DATE                  
*                                                                               
PROC10   MVC   THREE,TRNSDATE                                                   
         CLI   PROGPROF,C'Y'      PROFILE TO USE INPUT DATE NOT                 
         BNE   PROC20              TRANSACTION DATE                             
         MVI   ELCODE,X'60'                                                     
         L     R4,ADTRANS                                                       
         SH    R4,DATADISP                                                      
         BAS   RE,GETEL                                                         
         BNE   PROC20                                                           
         USING TRSTATD,R4                                                       
         GOTO1 DATCON,DMCB,(2,TRSTDATE),(1,THREE)                               
*                                                                               
PROC20   CLC   PENDDT,THREE                                                     
         BL    EXIT                                                             
         TM    TRNSSTAT,X'80'                                                   
         BNZ   EXIT                                                             
         SH    R9,DATADISP                                                      
         OC    ACDTUSED-ACKEYD(L'ACDTUSED,R9),ACDTUSED-ACKEYD(R9)               
         BZ    PROC40              UNDISBURSED - CARRY ON                       
*                                                                               
PROC25   CLI   QOPT7,C'Y'          IF DISBURSED AND NO OPTION TO RUN            
         BNE   EXIT                BEFORE CHEQUES - EXIT                        
         CLC   TODAY2,ACDTUSED-ACKEYD(R9)                                       
         BH    EXIT                                                             
         B     PROC40                                                           
*                                                                               
PROC30   TM    TRNSSTAT,X'80'      FOR MOS=N OR L TEST IF ITEM IS               
         BO    EXIT                PRESENTLY UNDISBURSED OR WAS                 
         SH    R9,DATADISP         UNDISBURSED AT THE END OF THE MONTH          
         USING ACKEYD,R9           IN QUESTION                                  
         OC    ACDTUSED,ACDTUSED                                                
         BZ    PROC40                                                           
*                                                                               
PROC35   CLC   ACDTUSED,DISDATE                                                 
         BH    PROC40                                                           
         B     EXIT                                                             
         DROP  R9                                                               
*                                                                               
PROC40   AH    R9,DATADISP         RESTORE R9                                   
         USING TRANSD,R9                                                        
         CLI   QOPT5,C' '                                                       
         BE    PROC60                                                           
         CLI   QOPT5,C'A'          APPROVED                                     
         BNE   PROC50                                                           
         TM    TRNSSTAT,X'02'                                                   
         BZ    EXIT                                                             
         B     PROC60                                                           
*                                                                               
PROC50   CLI   QOPT5,C'U'                                                       
         BNE   PROC60                                                           
         TM    TRNSSTAT,X'40'      URGENT                                       
         BZ    EXIT                                                             
         B     PROC60                                                           
*                                                                               
PROC60   OC    TRNSANAL,SPACES                                                  
         CLC   RQOFFICE,SPACES     TEST FOR OFFICE CODE                         
         BE    PROC70                                                           
         CLC   RQOFFICE,TRNSANAL                                                
         BNE   EXIT                                                             
*                                                                               
PROC70   CLI   QOPT6,C' '          OPTION TO IGNORE UNAUTH                      
         BE    PROC80                                                           
         CLI   QOPT6,C'U'                                                       
         BNE   PROC80                                                           
         TM    TRNSSTAT,X'08'      IF ON - TRANS IS AUTHORISED                  
         BZ    EXIT                                                             
         B     PROC80                                                           
*                                                                               
PROC80   CLI   PROGPROF+2,C'Y'                                                  
         BNE   PROC90                                                           
         MVI   ACTIVE,C'Y'                                                      
         MVI   CACTIVE,C'Y'                                                     
         BAS   RE,MERGINV                                                       
         B     EXIT                                                             
*                                                                               
PROC90   MVC   INVNUM,TRNSREF                                                   
         MVC   BATREF,TRNSBTCH                                                  
         EDIT  (P6,TRNSAMNT),(13,PAYAM),2,COMMAS=YES,MINUS=YES                  
         GOTO1 DATCON,DMCB,(1,TRNSDATE),(8,INVDATE)                             
         LA    RF,ACAUTH                                                        
         TM    TRNSSTAT,X'08'                                                   
         BO    *+8                                                              
         LA    RF,ACUNAUTH                                                      
         AP    0(6,RF),TRNSAMNT                                                 
*                                                                               
         CLI   PROGPROF,C'Y'      AGE USING CLEARED DATE                        
         BNE   PROC100                                                          
         MVI   ELCODE,X'60'        USE 60 EL IF POSSIBLE                        
         L     R4,ADTRANS                                                       
         BAS   RE,FIRSTEL                                                       
         BNE   PROC100             ELSE TRNSDATE IS OK                          
         USING TRSTATD,R4                                                       
         GOTO1 DATCON,DMCB,(2,TRSTDATE),(0,INVDT)                               
         B     PROC110                                                          
*                                                                               
PROC100  GOTO1 DATCON,DMCB,(1,TRNSDATE),(0,INVDT)                               
PROC110  GOTO1 =V(PERVERT),DMCB,INVDT,ENDDT,RR=RB                               
         LH    R1,DMCB+8                                                        
         LTR   R1,R1                                                            
         BM    *+10                                                             
         BCTR  R1,0                                                             
         B     *+8                                                              
         AH    R1,=H'1'                                                         
         EDIT  (R1),(4,AGE),MINUS=YES                                           
         ZIC   R2,NUMPARTS                                                      
         LA    R4,ACPART1                                                       
         LA    R5,PART1W                                                        
         SR    R3,R3                                                            
         LA    R6,ACPART1N                                                      
*                                                                               
PROC120  IC    R3,0(R5)            FIND CORRECT AGE BUCKETS                     
         SR    R1,R3                                                            
         LTR   R1,R1                                                            
         BM    PROC130                                                          
         LA    R4,6(R4)                                                         
         LA    R5,1(R5)                                                         
         LA    R6,2(R6)                                                         
         BCT   R2,PROC120                                                       
         LA    R4,ACOVER                                                        
         LA    R6,ACOVERN                                                       
*                                                                               
PROC130  AP    0(6,R4),TRNSAMNT(6)                                              
         LA    R1,ACPART1                                                       
         LA    R2,RQPART1                                                       
         SR    R4,R1                                                            
         LA    R2,0(R4,R2)                                                      
         AP    0(6,R2),TRNSAMNT                                                 
         AP    ACACTOT,TRNSAMNT                                                 
         AP    RQACTOT,TRNSAMNT                                                 
*                                                                               
         SR    R1,R1                                                            
         TM    TRNSSTAT,X'02'      CHECK IF AN APPROVED ITEM                    
         BZ    PROC135                                                          
         AP    ACAPTOT,TRNSAMNT    ADD TO ACCUMS                                
         LH    R1,ACAPTOTN                                                      
         AH    R1,=H'1'                                                         
         STH   R1,ACAPTOTN         BUMP ITEM COUNT                              
*                                                                               
PROC135  TM    TRNSSTAT,X'04'      CHECK IF A HELD ITEM                         
         BZ    PROC140                                                          
         AP    ACHDTOT,TRNSAMNT                                                 
         LH    R1,ACHDTOTN                                                      
         AH    R1,=H'1'                                                         
         STH   R1,ACHDTOTN                                                      
*                                                                               
PROC140  BAS   RE,ADDRTN           UPDATE ACPART1N ETC                          
         LA    R5,RQPART1N                                                      
         LA    R4,ACPART1N                                                      
         SR    R6,R4                                                            
         AR    R6,R5                                                            
         BAS   RE,ADDRTN                                                        
         LA    R6,ACACTOTN                                                      
         BAS   RE,ADDRTN                                                        
         LA    R6,RQACTOTN                                                      
         BAS   RE,ADDRTN                                                        
         MVI   ACTIVE,C'Y'                                                      
         MVI   CACTIVE,C'Y'                                                     
         MVC   P+1(L'DETAIL),DETAIL                                             
         BAS   RE,OTHERDIG                                                      
         GOTO1 AXREPORT                                                         
         MVC   DETAIL,SPACES                                                    
         B     EXIT                                                             
*                                                                               
ADDRTN   LH    R1,0(R6)            ADD 1 TO HALF WORD 0(R6)                     
         AH    R1,=H'1'                                                         
         STH   R1,0(R6)                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        ACCLAST                                                      *         
***********************************************************************         
*                                                                               
ACLAST   DS    0H                                                               
         CLI   ACTIVE,C'N'                                                      
         BE    ACLST50                                                          
         GOTO1 AXREPORT            SKIP A LINE                                  
         CLI   PROGPROF+2,C'Y'                                                  
         BNE   ACLST10                                                          
         CLI   CACTIVE,C'N'                                                     
         BE    ACLST10                                                          
         MVI   LASTSW,1                                                         
         BAS   RE,MERGINV                                                       
*                                                                               
ACLST10  ZIC   R1,NUMPARTS                                                      
         LA    R1,11(R1)                                                        
         ZIC   R2,LINE             CAN I FIT THE SUMMARY ON THIS PAGE           
         AR    R2,R1                                                            
         ZIC   R1,MAXLINES                                                      
         CR    R2,R1                                                            
         BL    *+8                 NO                                           
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   PROGPROF+3,C'Y'    PRINT HELD/APPROVED TOTALS                    
         BNE   ACLST16                                                          
         CP    ACHDTOT,=P'0'                                                    
         BE    ACLST14                                                          
         MVC   P+62(10),=C'TOTAL HELD'                                          
         MVC   P+83(5),=C'ITEMS'                                                
         EDIT  (P6,ACHDTOT),(13,P+90),2,COMMAS=YES,MINUS=YES                    
         EDIT  (B2,ACHDTOTN),(4,P+78)                                           
         LH    R1,RQHDTOTN                                                      
         AH    R1,ACHDTOTN                                                      
         STH   R1,RQHDTOTN                                                      
         XC    ACHDTOTN,ACHDTOTN                                                
         AP    RQHDTOT,ACHDTOT                                                  
         MVI   PSECOND+62,BOTF                                                  
         CLI   REMOTOP,C'Y'                                                     
         BNE   *+8                                                              
         MVI   PSECOND+62,C'-'                                                  
         MVC   PSECOND+63(12),PSECOND+62                                        
         GOTO1 AXREPORT                                                         
*                                                                               
ACLST14  CP    ACAPTOT,=P'0'                                                    
         BE    ACLST16                                                          
         MVC   P+62(14),=C'TOTAL APPROVED'                                      
         MVC   P+83(5),=C'ITEMS'                                                
         EDIT  (P6,ACAPTOT),(13,P+90),2,COMMAS=YES,MINUS=YES                    
         EDIT  (B2,ACAPTOTN),(4,P+78)                                           
         LH    R1,RQAPTOTN                                                      
         AH    R1,ACAPTOTN                                                      
         STH   R1,RQAPTOTN                                                      
         XC    ACAPTOTN,ACAPTOTN                                                
         AP    RQAPTOT,ACAPTOT                                                  
         MVI   PSECOND+62,BOTF                                                  
         CLI   REMOTOP,C'Y'                                                     
         BNE   *+8                                                              
         MVI   PSECOND+62,C'-'                                                  
         MVC   PSECOND+63(12),PSECOND+62                                        
         GOTO1 AXREPORT                                                         
*                                                                               
ACLST16  MVC   P+62(13),=C'ACCOUNT TOTAL'                                       
         MVI   PSECOND+62,BOTF                                                  
         CLI   REMOTOP,C'Y'                                                     
         BNE   *+8                                                              
         MVI   PSECOND+62,C'-'                                                  
         MVC   PSECOND+63(12),PSECOND+62                                        
         LA    R1,P                                                             
         MVI   SPACING,X'02'                                                    
         B     ACLST30                                                          
*                                                                               
ACLST20  MVI   RCSUBPRG,X'02'                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVI   HEAD12+62,BOTF                                                   
         CLI   REMOTOP,C'Y'                                                     
         BNE   *+8                                                              
         MVI   HEAD12+62,C'-'                                                   
         MVC   HEAD12+63(12),HEAD12+62                                          
         LA    R1,HEAD11                                                        
         MVI   SPACING,X'01'                                                    
*                                                                               
ACLST30  EDIT  (P6,ACACTOT),(13,90(R1)),2,COMMAS=YES,MINUS=YES                  
         EDIT  (B2,ACACTOTN),(4,78(R1))                                         
         MVC   83(5,R1),=C'ITEMS'                                               
         GOTO1 AXREPORT                                                         
         MVC   P+62(15),=C'ACCOUNT SUMMARY'                                     
         MVI   PSECOND+62,BOTF                                                  
         CLI   REMOTOP,C'Y'                                                     
         BNE   *+8                                                              
         MVI   PSECOND+62,C'-'                                                  
         MVC   PSECOND+63(14),PSECOND+62                                        
         GOTO1 AXREPORT                                                         
         MVI   SUMSW,C'N'                                                       
         BAS   RE,SUMRTN                                                        
         CLI   SYSPROF+1,C'Y'                                                   
         BNE   ACLST40                                                          
         GOTO1 AXREPORT                                                         
         MVC   P+62(16),=C'AUTHORISED TOTAL'                                    
         EDIT  ACAUTH,(13,P+90),2,MINUS=YES                                     
         GOTO1 AXREPORT                                                         
         MVC   P+62(18),=C'UNAUTHORISED TOTAL'                                  
         EDIT  ACUNAUTH,(13,P+90),2,MINUS=YES                                   
         GOTO1 AXREPORT                                                         
*                                                                               
ACLST40  MVI   FORCEHED,C'Y'                                                    
*                                                                               
ACLST50  MVC   HEAD7+10(L'SAVEAC),SPACES                                        
         MVI   ACTIVE,C'N'                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        REQUEST LAST                                                 *         
***********************************************************************         
*                                                                               
REQL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,X'01'                                                   
         MVI   SUMSW,C'Y'                                                       
*                                                                               
         CLI   PROGPROF+3,C'Y'    PRINT HELD/APPROVED TOTALS                    
         BNE   REQL30                                                           
         CP    RQHDTOT,=P'0'                                                    
         BE    REQL10                                                           
         GOTO1 AXREPORT                                                         
         MVC   P+62(10),=C'TOTAL HELD'                                          
         MVC   P+83(5),=C'ITEMS'                                                
         EDIT  (P6,RQHDTOT),(13,P+90),2,COMMAS=YES,MINUS=YES                    
         EDIT  (B2,RQHDTOTN),(4,P+78)                                           
         MVI   PSECOND+62,BOTF                                                  
         CLI   REMOTOP,C'Y'                                                     
         BNE   *+8                                                              
         MVI   PSECOND+62,C'-'                                                  
         MVC   PSECOND+63(12),PSECOND+62                                        
         GOTO1 AXREPORT                                                         
*                                                                               
REQL10   CP    RQAPTOT,=P'0'                                                    
         BE    REQL30                                                           
         MVC   P+62(14),=C'TOTAL APPROVED'                                      
         MVC   P+83(5),=C'ITEMS'                                                
         EDIT  (P6,RQAPTOT),(13,P+90),2,COMMAS=YES,MINUS=YES                    
         EDIT  (B2,RQAPTOTN),(4,P+78)                                           
         MVI   PSECOND+62,BOTF                                                  
         CLI   REMOTOP,C'Y'                                                     
         BNE   *+8                                                              
         MVI   PSECOND+62,C'-'                                                  
         MVC   PSECOND+63(12),PSECOND+62                                        
         GOTO1 AXREPORT                                                         
*                                                                               
REQL30   GOTO1 AXREPORT                                                         
         MVC   P+62(13),=C'REQUEST TOTAL'                                       
         MVI   PSECOND+62,BOTF                                                  
         CLI   REMOTOP,C'Y'                                                     
         BNE   *+8                                                              
         MVI   PSECOND+62,C'-'                                                  
         MVC   PSECOND+63(12),PSECOND+62                                        
         MVI   SPACING,X'02'                                                    
         OC    RQACTOTN,RQACTOTN                                                
         BNZ   REQL40                                                           
         MVC   P+101(3),=C'NIL'                                                 
         B     REQL50                                                           
*                                                                               
REQL40   DS    0H                                                               
         EDIT  (P6,RQACTOT),(13,P+90),2,COMMAS=YES,MINUS=YES                    
         EDIT  (B2,RQACTOTN),(4,P+78)                                           
         MVC   P+83(5),=C'ITEMS'                                                
*                                                                               
REQL50   MVI   SPACING,X'01'                                                    
         MVI   HEAD12+62,BOTF                                                   
         CLI   REMOTOP,C'Y'                                                     
         BNE   *+8                                                              
         MVI   HEAD12+62,C'-'                                                   
         MVC   HEAD12+63(12),HEAD12+62                                          
         GOTO1 AXREPORT                                                         
         GOTO1 AXREPORT            SKIP ONE LINE                                
*                                                                               
         MVC   P+62(15),=C'REQUEST SUMMARY'                                     
         MVI   PSECOND+62,BOTF                                                  
         CLI   REMOTOP,C'Y'                                                     
         BNE   *+8                                                              
         MVI   PSECOND+62,C'-'                                                  
         MVC   PSECOND+63(14),PSECOND+62                                        
         GOTO1 AXREPORT                                                         
         BAS   RE,SUMRTN                                                        
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SUMRTN                                                       *         
***********************************************************************         
*                                                                               
SUMRTN   NTR1                                                                   
         MVI   FINISH,C'N'                                                      
         CLI   SUMSW,C'Y'                                                       
         BE    SUM10                                                            
         LA    R1,ACPART1                                                       
         LA    R6,ACPART1N                                                      
         B     SUM20                                                            
*                                                                               
SUM10    LA    R1,RQPART1                                                       
         LA    R6,RQPART1N                                                      
*                                                                               
SUM20    ZIC   R3,NUMPARTS                                                      
         LA    R2,PART1W                                                        
         SR    R4,R4                                                            
         MVI   SPACING,X'01'                                                    
*                                                                               
SUM50    MVC   P+66(L'MESSA),MESSA                                              
         EDIT  (R4),(3,P+63)                                                    
         OI    P+65,X'F0'                                                       
         ZIC   R5,0(R2)                                                         
         AR    R4,R5                                                            
         BCTR  R4,0                                                             
         EDIT  (R4),(3,P+67),ALIGN=LEFT                                         
*                                                                               
SUM60    LH    R7,0(R6)                                                         
         CH    R7,=H'0'                                                         
         BNE   SUM70                                                            
         MVC   P+99(3),=C'NIL'                                                  
         B     SUM80                                                            
*                                                                               
SUM70    EDIT  (P6,0(R1)),(13,P+90),2,COMMAS=YES,MINUS=YES                      
         EDIT  (B2,0(R6)),(4,P+78)                                              
         MVC   P+83(5),=C'ITEMS'                                                
*                                                                               
SUM80    GOTO1 AXREPORT                                                         
         LA    R4,1(R4)                                                         
         LA    R2,1(R2)                                                         
         LA    R1,6(R1)                                                         
         LA    R6,2(R6)                                                         
         BCT   R3,SUM50                                                         
         CLI   FINISH,C'Y'                                                      
         BE    EXIT                                                             
         CLI   SUMSW,C'Y'                                                       
         BE    SUM90                                                            
         LA    R1,ACOVER                                                        
         LA    R6,ACOVERN                                                       
         B     SUM100                                                           
*                                                                               
SUM90    LA    R1,RQOVER                                                        
         LA    R6,RQOVERN                                                       
*                                                                               
SUM100   MVC   P+69(L'MESSB),MESSB                                              
         EDIT  (R4),(3,P+66)                                                    
         LA    R3,1                                                             
         MVI   FINISH,C'Y'                                                      
         B     SUM60                                                            
         EJECT                                                                  
***********************************************************************         
*        AXREPORT                                                     *         
***********************************************************************         
*                                                                               
AXREPORT NTR1                                                                   
         MVC   HEAD5+89(L'ENDDAY),ENDDAY                                        
         CLI   RCSUBPRG,1                                                       
         BE    *+10                                                             
         MVC   HEAD6+10(L'SAVEAC),SAVEAC                                        
         CLC   RQOFFICE,SPACES                                                  
         BE    AX10                                                             
*&&US*&& MVC   HEAD6+69(L'SAVEOFF),SAVEOFF                                      
*&&UK*&& MVC   HEAD7+1(L'SAVEOFF),SAVEOFF                                       
         B     AX20                                                             
*                                                                               
AX10     DS    0H                                                               
*&&US*&& MVC   HEAD6+69(L'SAVEOFF),SPACES                                       
*&&UK*&& MVC   HEAD7+1(L'SAVEOFF),SPACES                                        
*                                                                               
AX20     CLI   RCSUBPRG,1                                                       
         BE    AX50                                                             
         CLI   PROGPROF+1,C'Y'    PROFILE TO PRINT ADDRESSES                    
         BNE   AX50                                                             
         L     R2,ADACCADD                                                      
         LTR   R2,R2                                                            
         BZ    AX50                                                             
         USING ACADDD,R2                                                        
         LA    RF,HEAD6+79                                                      
         ZIC   RE,ACADLNES                                                      
         LA    R1,ACADADD                                                       
*                                                                               
AX40     MVC   0(L'ACADADD,RF),0(R1)                                            
         LA    RF,L'HEAD7(RF)                                                   
         LA    R1,L'ACADADD(R1)                                                 
         BCT   RE,AX40                                                          
*                                                                               
AX50     GOTO1 ACREPORT                                                         
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        GET OTHER ELEMENTS                                           *         
***********************************************************************         
*                                                                               
OTHERDIG NTR1                                                                   
         L     R9,ADTRANS                                                       
         USING TRANSD,R9                                                        
         MVI   CHOPWRK,C' '                                                     
         MVC   CHOPWRK+1(L'CHOPWRK-1),CHOPWRK                                   
         ZIC   RF,TRNSLEN                                                       
         SH    RF,=H'29'           PUT NARRATIVE IN AS A DEFAULT                
         LTR   RF,RF                                                            
         BM    OTH2                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CHOPWRK(0),TRNSNARR                                              
*                                                                               
OTH2     CLI   0(R9),0                                                          
         BE    OTHXT                                                            
         CLI   0(R9),X'23'                                                      
         BE    OTH10                                                            
         CLI   0(R9),X'47'                                                      
         BE    OTH20                                                            
         CLI   0(R9),X'46'                                                      
         BE    OTH30                                                            
         CLI   0(R9),X'4F'                                                      
         BE    OTH40                                                            
         CLI   0(R9),X'50'                                                      
         BE    OTH50                                                            
OTH4     ZIC   RF,1(R9)                                                         
         AR    R9,RF                                                            
         B     OTH2                                                             
*                                                                               
         USING ACOTHERD,R9                                                      
OTH10    LA    RF,CHOPWRK+L'CHOPWRK-1                                           
         CLI   0(RF),C' '                                                       
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,1(RF)                                                         
*&&US                                                                           
         MVC   1(5,RF),=C'PROD='                                                
         MVC   13(4,RF),=C'JOB='                                                
         MVC   6(6,RF),ACOTNUM     PRODUCT                                      
         MVC   17(6,RF),ACOTNUM+6  JOB                                          
*&&                                                                             
*&&UK                                                                           
         MVC   1(7,RF),=C'SUBREF='                                              
         MVC   8(6,RF),ACOTNUM                                                  
*&&                                                                             
         B     OTH4                                                             
*                                                                               
         USING TRPUKD,R9                                                        
OTH20    MVI   CHOPWRK,C' '                                                     
         MVC   CHOPWRK+1(L'CHOPWRK-1),CHOPWRK                                   
         MVC   CHOPWRK(1),TRPKSER                                               
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),TRPKSER+1                                              
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         UNPK  CHOPWRK+1(7),DUB                                                 
         OI    CHOPWRK+7,X'F0'                                                  
         GOTO1 DATCON,DMCB,(1,TRPKIDAT),(8,CHOPWRK+10)                          
         LA    R2,CHOPWRK+20                                                    
         OC    TRPKGRSS,TRPKGRSS                                                
         BZ    OTH21                                                            
         EDIT  TRPKGRSS,(12,0(R2)),2,MINUS=YES                                  
*                                                                               
OTH21    MVC   CHOPWRK+35(L'TRPKCLI),TRPKCLI                                    
         MVC   CHOPWRK+50(L'TRPKPROD),TRPKPROD                                  
         MVC   CHOPWRK+65(L'TRPKREM),TRPKREM                                    
*                                                                               
         L     RF,ADACC                                                         
         CLI   4(RF),C'T'                                                       
         BNE   OTH22                                                            
         LA    R2,CHOPWRK+85                                                    
         EDIT  (B2,TRPKSEC),(3,(R2))                                            
         LA    R2,4(R2)                                                         
         OC    TRPKTIM,TRPKTIM                                                  
         BZ    OTH23                                                            
         GOTO1 =V(TMUNPK),DMCB,TRPKTIM,(R2),RR=RB                               
         B     OTH23                                                            
*                                                                               
OTH22    MVC   CHOPWRK+85(L'TRPKPOS),TRPKPOS                                    
         MVC   CHOPWRK+90(L'TRPKCOL),TRPKCOL                                    
         CLI   TRPKCOL+2,C'C'                                                   
         BNE   OTH23                                                            
         MVC   CHOPWRK+90(2),CHOPWRK+91                                         
         MVC   CHOPWRK+92(2),=C'OL'                                             
         B     OTH23                                                            
*                                                                               
OTH23    CLI   TRPKLEN,TRPKLENQ                                                 
         BL    OTHXT                                                            
         CP    TRPKSLSH,=P'0'                                                   
         BE    OTHXT                                                            
         MVC   CHOPWRK+100(11),=C'OVER/UNDER='                                  
         LA    RF,CHOPWRK+111                                                   
         EDIT  TRPKSLSH,(10,0(RF)),2,MINUS=YES,ALIGN=LEFT                       
         B     OTHXT                                                            
*                                                                               
         USING TRPAYD,R9                                                        
OTH30    MVI   CHOPWRK,C' '                                                     
         MVC   CHOPWRK+1(L'CHOPWRK-1),CHOPWRK                                   
         CP    TRPYCD,=P'0'                                                     
         BE    OTH32                                                            
         LA    R2,CHOPWRK+3                                                     
         MVC   CHOPWRK(3),=C'CD='                                               
         EDIT  TRPYCD,(10,0(R2)),2,MINUS=YES,ALIGN=LEFT                         
*                                                                               
OTH32    MVC   CHOPWRK+20(L'TRPYCLI),TRPYCLI                                    
         MVC   CHOPWRK+45(L'TRPYPROD),TRPYPROD                                  
         MVC   CHOPWRK+70(L'TRPYINV),TRPYINV                                    
         CLI   TRPYPER,X'41'                                                    
         BL    OTHXT                                                            
         CLC   TRPYPER+4(2),=C'00'                                              
         BNE   *+8                                                              
         MVI   TRPYPER+5,C'1'                                                   
         GOTO1 DATCON,DMCB,(0,TRPYPER),(8,CHOPWRK+90)                           
         CLI   TRPYPER+6,X'41'                                                  
         BL    OTHXT                                                            
         MVI   CHOPWRK+99,C'-'                                                  
         GOTO1 (RF),(R1),(0,TRPYPER+6),(8,CHOPWRK+101)                          
         B     OTHXT                                                            
*                                                                               
         USING TRCPJD,R9                                                        
OTH40    LA    RF,CHOPWRK+L'CHOPWRK-1                                           
         CLI   0(RF),C' '                                                       
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,2(RF)                                                         
         MVC   0(4,RF),=C'JOB='                                                 
         CLI   TRCPTYPE,C'J'                                                    
         BE    *+10                                                             
         MVC   0(3,RF),=C'EXP'                                                  
         MVC   4(18,RF),TRCPCLI                                                 
         B     OTH4                                                             
*                                                                               
         USING TRCASHD,R9                                                       
OTH50    CLI   TRCSTYPE,C'D'                                                    
         BNE   OTHXT                                                            
         LA    RF,CHOPWRK+L'CHOPWRK-1                                           
         CLI   0(RF),C' '                                                       
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,2(RF)                                                         
         MVC   0(5,RF),=C'DISC='                                                
         EDIT  (P6,TRCSAMNT),(9,5(RF)),2,MINUS=YES,ALIGN=LEFT                   
         B     OTH4                                                             
*                                                                               
OTHXT    DS    0H                                                               
         LA    RF,CHOPWRK+L'CHOPWRK-1                                           
         CLI   0(RF),C' '                                                       
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,2(RF)                                                         
         L     R9,ADTRANS                                                       
         USING TRANSD,R9                                                        
         TM    TRNSSTAT,X'40'                                                   
         BZ    *+14                                                             
         MVC   0(6,RF),=C'URGENT'                                               
         LA    RF,7(RF)                                                         
         TM    TRNSSTAT,X'02'                                                   
         BZ    *+10                                                             
         MVC   0(8,RF),=C'APPROVED'                                             
         LA    RF,9(RF)                                                         
         TM    TRNSSTAT,X'04'                                                   
         BZ    *+10                                                             
         MVC   0(4,RF),=C'HELD'                                                 
         LA    RF,6(RF)                                                         
         CLI   SYSPROF+1,C'Y'      INVOICE REGISTER PROFILE                     
         BNE   OTHXT2                                                           
         CLI   MEDSW,C'Y'          SKIP IF A MEDIA LEDGER                       
         BE    OTHXT2                                                           
         TM    TRNSSTAT,X'08'      AUTHORISED BIT                               
         BO    OTHXT2                                                           
         MVC   0(6,RF),=C'UNAUTH'                                               
         LA    RF,7(RF)                                                         
*                                                                               
OTHXT2   DS    0H                                                               
         GOTO1 =V(SQUASHER),DMCB,CHOPWRK,164,RR=RB                              
         L     R4,DMCB+4                                                        
         GOTO1 CHOPPER,DMCB,((R4),CHOPWRK),(41,P+46),(C'P',4)                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        MERGE ROUTINE                                                *         
***********************************************************************         
*                                                                               
MERGINV  NTR1                                                                   
         CLI   FIRSTSW,0                                                        
         BE    MG0100                                                           
         CLI   LASTSW,1                                                         
         BE    MG0500                                                           
         CLC   INVNUM(6),TRNSREF                                                
         BNE   MG0500                                                           
         CLC   SVTRNSDT(3),TRNSDATE                                             
         BNE   MG0500                                                           
*                                                                               
MG0100   MVI   FIRSTSW,1                                                        
*                                                                               
MG0200   AP    ITEMCNT,=P'1'                                                    
         MVC   INVNUM,TRNSREF                                                   
         MVC   BATREF,TRNSBTCH                                                  
         AP    SVTRNSAM,TRNSAMNT                                                
         MVC   SVTRNSDT,TRNSDATE                                                
         MVC   SVTRNSTA,TRNSSTAT                                                
         GOTO1 DATCON,DMCB,(1,TRNSDATE),(8,INVDATE)                             
         LA    RF,ACAUTH                                                        
         TM    TRNSSTAT,X'08'                                                   
         BO    *+8                                                              
         LA    RF,ACUNAUTH                                                      
         AP    0(6,RF),TRNSAMNT                                                 
         CLI   PROGPROF,C'Y'      AGE USING CLEARED DATE                        
         BNE   MG0300                                                           
         MVI   ELCODE,X'60'        USE 60 EL IF POSSIBLE                        
         L     R4,ADTRANS                                                       
         BAS   RE,FIRSTEL                                                       
         BNE   MG0300              ELSE TRNSDATE IS OK                          
         USING TRSTATD,R4                                                       
         GOTO1 DATCON,DMCB,(2,TRSTDATE),(0,INVDT)                               
         B     MG0400                                                           
*                                                                               
MG0300   GOTO1 DATCON,DMCB,(1,TRNSDATE),(0,INVDT)                               
*                                                                               
MG0400   GOTO1 =V(PERVERT),DMCB,INVDT,ENDDT,RR=RB                               
         LH    R1,DMCB+8                                                        
         LTR   R1,R1                                                            
         BM    *+10                                                             
         BCTR  R1,0                                                             
         B     *+8                                                              
         AH    R1,=H'1'                                                         
         EDIT  (R1),(4,AGE),MINUS=YES                                           
         STH   R1,SAVER1                                                        
         BAS   RE,OTHERDIG                                                      
         MVC   OTHER(41),P+46                                                   
         B     EXIT                                                             
*                                                                               
MG0500   CP    ITEMCNT,=P'1'                                                    
         BE    MG0600                                                           
         MVC   BATREF,SPACES                                                    
         MVC   OTHER,SPACES                                                     
         EDIT  (P3,ITEMCNT),(5,OTHER),ALIGN=LEFT                                
         LA    R2,OTHER                                                         
         LA    R2,1(R2)                                                         
         CLI   0(R2),C' '                                                       
         BNE   *-8                                                              
         MVC   1(15,R2),=C'MERGED INVOICES'                                     
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         MVC   PFOURTH,SPACES                                                   
*                                                                               
MG0600   EDIT  (P6,SVTRNSAM),(13,PAYAM),2,COMMAS=YES,MINUS=YES                  
         MVC   P+1(L'DETAIL),DETAIL                                             
         GOTO1 AXREPORT                                                         
         MVC   DETAIL,SPACES                                                    
         MVC   SVTRNSDT,SPACES                                                  
         ZAP   ITEMCNT,=P'0'                                                    
         LH    R1,SAVER1                                                        
         ZIC   R2,NUMPARTS                                                      
         LA    R4,ACPART1                                                       
         LA    R5,PART1W                                                        
         SR    R3,R3                                                            
         LA    R6,ACPART1N                                                      
*                                                                               
MG0700   IC    R3,0(R5)            FIND CORRECT AGE BUCKETS                     
         SR    R1,R3                                                            
         LTR   R1,R1                                                            
         BM    MG0800                                                           
         LA    R4,6(R4)                                                         
         LA    R5,1(R5)                                                         
         LA    R6,2(R6)                                                         
         BCT   R2,MG0700                                                        
         LA    R4,ACOVER                                                        
         LA    R6,ACOVERN                                                       
*                                                                               
MG0800   AP    0(6,R4),SVTRNSAM(6)                                              
         LA    R1,ACPART1                                                       
         LA    R2,RQPART1                                                       
         SR    R4,R1                                                            
         LA    R2,0(R4,R2)                                                      
         AP    0(6,R2),SVTRNSAM                                                 
         AP    ACACTOT,SVTRNSAM                                                 
         AP    RQACTOT,SVTRNSAM                                                 
*                                                                               
         SR    R1,R1                                                            
         TM    SVTRNSTA,X'02'      CHECK IF AN APPROVED ITEM                    
         BZ    MG0900                                                           
         AP    ACAPTOT,SVTRNSAM    ADD TO ACCUMS                                
         LH    R1,ACAPTOTN                                                      
         AH    R1,=H'1'                                                         
         STH   R1,ACAPTOTN         BUMP ITEM COUNT                              
*                                                                               
MG0900   TM    SVTRNSTA,X'04'      CHECK IF A HELD ITEM                         
         BZ    MG1000                                                           
         AP    ACHDTOT,SVTRNSAM                                                 
         LH    R1,ACHDTOTN                                                      
         AH    R1,=H'1'                                                         
         STH   R1,ACHDTOTN                                                      
*                                                                               
MG1000   BAS   RE,MGADDRTN         UPDATE ACPART1N ETC                          
         LA    R5,RQPART1N                                                      
         LA    R4,ACPART1N                                                      
         SR    R6,R4                                                            
         AR    R6,R5                                                            
         BAS   RE,MGADDRTN                                                      
         LA    R6,ACACTOTN                                                      
         BAS   RE,MGADDRTN                                                      
         LA    R6,RQACTOTN                                                      
         BAS   RE,MGADDRTN                                                      
         ZAP   SVTRNSAM,=P'0'                                                   
         CLI   LASTSW,1                                                         
         BE    EXIT                                                             
         B     MG0200                                                           
*                                                                               
MGADDRTN LH    R1,0(R6)            ADD 1 TO HALF WORD 0(R6)                     
         AH    R1,=H'1'                                                         
         STH   R1,0(R6)                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS/LTORG/DSECTS                                       *         
***********************************************************************         
*                                                                               
DEF      DC    AL1(4,11,10,10,10,0)     DEFAULT                                 
OPTA     DC    AL1(3,11,10,10,0,0)      OPTION A                                
OPTB     DC    AL1(3,10,10,10,0,0)      OPTION B                                
OPTC     DC    AL1(3,11,20,30,0,0)      OPTION C                                
*                                                                               
MESSA    DC    C'-    DAYS'                                                     
MESSB    DC    C'&& OVER'                                                       
*                                                                               
*&&US                                                                           
MEDLEG   DS    0CL1                MEDIA LEDGERS                                
         DC    C'S'                                                             
         DC    C'P'                                                             
         DC    C'T'                                                             
         DC    C'Q'                                                             
         DC    X'FF'                                                            
*&&                                                                             
*                                                                               
*&&UK                                                                           
MEDLEG   DS    0CL1                MEDIA LEDGERS                                
         DC    C'F'                                                             
         DC    X'FF'                                                            
*&&                                                                             
MEDSW    DS    CL1                 Y=MEDIA LEDGER, N=NOT A MEDIA LEDGER         
         EJECT                                                                  
***********************************************************************         
*        LITERAL POOL                                                 *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        HOOK                                                         *         
***********************************************************************         
*                                                                               
         ENTRY HOOK                                                             
HOOK     NMOD1 0,*HOOK*                                                         
         L     RC,SAVERC                                                        
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   MYCOL,SPACES                                                     
         MVC   MYROW,SPACES                                                     
         CLI   MODE,REQLAST        REQUEST TOTAL PAGE                           
         BE    HOOK100                                                          
         MVI   MYCOL,C'L'                                                       
         MVI   MYCOL+17,C'C'                                                    
         MVI   MYCOL+33,C'C'                                                    
         MVI   MYCOL+45,C'C'                                                    
         MVI   MYCOL+89,C'C'                                                    
         MVI   MYCOL+104,C'C'                                                   
         MVI   MYCOL+109,C'R'                                                   
         MVI   MYROW+9,C'T'                                                     
         MVI   MYROW+12,C'M'                                                    
         B     HOOK200                                                          
*                                                                               
HOOK100  MVI   MYCOL+55,C'L'                                                    
         MVI   MYCOL+89,C'C'                                                    
         MVI   MYCOL+104,C'R'                                                   
         MVI   MYROW+6,C'T'                                                     
*                                                                               
HOOK200  MVI   MYROW+56,C'B'                                                    
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVC   BOXCOLS,MYCOL                                                    
         MVC   BOXROWS,MYROW                                                    
*                                                                               
HOOKX    XMOD1 1                                                                
SAVERC   DC    A(0)                                                             
         EJECT                                                                  
***********************************************************************         
*        LOCAL WORKING STORAGE                                        *         
***********************************************************************         
*                                                                               
ACC1D    DSECT                                                                  
DETAIL   DS    0CL107              MOVE TO P+1(L'DETAIL)                        
CNTRAC   DS    CL14                                                             
         DS    CL3                                                              
INVNUM   DS    CL6                                                              
         DS    CL1                                                              
INVDATE  DS    CL8                                                              
         DS    CL4                                                              
BATREF   DS    CL6                                                              
         DS    CL4                                                              
OTHER    DS    CL41                                                             
         DS    CL2                                                              
PAYAM    DS    CL13                                                             
         DS    CL2                                                              
AGE      DS    CL4                                                              
SVTRNSTA DS    XL1                 SAVED STATUS FOR MERGED ITEMS                
SVTRNSDT DS    CL3                 SAVED DATE FOR MERGED ITEMS                  
SVTRNSAM DS    PL6                 SAVED AMOUNT FOR MERGED ITEMS                
ITEMCNT  DS    PL3                 MERGED ITEM COUNTER                          
SAVER1   DS    H                   R1 SAVED FOR MERGED INVOICE AGEING           
RQOFFICE DS    CL2                                                              
         DS    0D                                                               
ACPART1  DS    PL6                 ACCOUNT ACCUMULATORS                         
ACPART2  DS    PL6                                                              
ACPART3  DS    PL6                                                              
ACPART4  DS    PL6                                                              
ACPART5  DS    PL6                                                              
ACOVER   DS    PL6                                                              
*                                                                               
RQPART1  DS    PL6                 REQUEST ACCUMULATORS                         
RQPART2  DS    PL6                                                              
RQPART3  DS    PL6                                                              
RQPART4  DS    PL6                                                              
RQPART5  DS    PL6                                                              
RQOVER   DS    PL6                                                              
*                                                                               
ACHDTOT  DS    PL6                HELD TOTAL                                    
ACAPTOT  DS    PL6                APPROVED TOTAL                                
ACACTOT  DS    PL6                 ACCT TOTAL                                   
RQHDTOT  DS    PL6                                                              
RQAPTOT  DS    PL6                                                              
RQACTOT  DS    PL6                                                              
ACAUTH   DS    PL6                                                              
ACUNAUTH DS    PL6                                                              
*                                                                               
ACPART1N DS    H                   NO OF ITEMS IN EACH CATEGORY                 
         DS    4H                                                               
ACOVERN  DS    H                                                                
*                                                                               
RQPART1N DS    H                                                                
         DS    4H                                                               
RQOVERN  DS    H                                                                
*                                                                               
ACHDTOTN DS    H                   NUMBER OF HELD ITEMS AT ACCT LEVEL           
ACAPTOTN DS    H                   NUMBER OF APPROVED ITEMS AT ACCT LEV         
ACACTOTN DS    H                   NUMBER OF ITEMS AT ACCT LEVEL                
*                                                                               
RQHDTOTN DS    H                   NUMBER OF HELD ITEMS AT REQ LEVEL            
RQAPTOTN DS    H                   NUMBER OF APPROVED ITEMS AT REQ LEV          
RQACTOTN DS    H                   NUMBER OF TOTAL ITEMS AT REQ LEV             
*                                                                               
NUMPARTS DS    CL1                 NO OF PARTS    DEFAULT 4                     
PART1W   DS    CL1                 WIDTH OF PART1        11                     
PART2W   DS    CL1                          PART2        10                     
PART3W   DS    CL1                          PART3        10                     
PART4W   DS    CL1                          PART4        10                     
PART5W   DS    CL1                                                              
*                                                                               
ENDDAY   DS    CL8                 PRINTABLE END DATE                           
ENDDT    DS    CL6                 6 BYTE EBCDIC END DATE                       
INVDT    DS    CL6                 6 BYTE EBCDIC INVOICE DATE                   
*                                                                               
PENDDT   DS    CL3                 3 BYTE PACKED END DATE                       
DISDATE  DS    CL2                                                              
TODAY2   DS    CL2                                                              
*                                                                               
SAVEAC   DS    CL50                ACCOUNT NAME AND NO. GO HERE                 
SAVEOFF  DS    CL39                                                             
ELCODE   DS    CL1                                                              
CHOPWRK  DS    CL164               4 X 41 FOR OTHER INFO                        
         DS    CL200                                                            
*                                                                               
* SWITCHES                                                                      
*                                                                               
OPTION   DS    CL1                                                              
ACTIVE   DS    CL1                 ACCOUNT ACTIVITY                             
CACTIVE  DS    CL1                 CONTRA ACTIVITY                              
FINISH   DS    CL1                                                              
SUMSW    DS    CL1                                                              
FIRSTSW  DS    XL1                                                              
LASTSW   DS    XL1                                                              
RELOC    DS    A                                                                
ADBOX    DS    A                                                                
MYROW    DS    CL100                                                            
MYCOL    DS    CL132                                                            
REMOTOP  DS    C                                                                
SAVEKEY  DS    CL15                                                             
MYKEY    DS    CL49                                                             
IOAREA   DS    CL1000                                                           
         EJECT                                                                  
***********************************************************************         
*        OTHER INCLUDES                                               *         
***********************************************************************         
*                                                                               
*        ACREPWORKD                                                             
*        ACGENBOTH                                                              
*        ACGENMODES                                                             
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039ACREPC102 05/01/02'                                      
         END                                                                    
