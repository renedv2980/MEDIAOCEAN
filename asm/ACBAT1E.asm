*          DATA SET ACBAT1E    AT LEVEL 008 AS OF 05/01/02                      
*PHASE T61B1EA                                                                  
*INCLUDE AMTVAL                                                                 
         TITLE 'ACBAT1E - CASH ALLOCATION'                                      
         PRINT NOGEN                                                            
T61B1E   CSECT                                                                  
         NMOD1 PROGDX-PROGD,**BAT1E,R8,RR=R5,CLEAR=YES                          
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         ST    RD,SAVERD                                                        
         ST    R5,MYRELO                                                        
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         EJECT                                                                  
*              VALIDATE OPTIONS/FILTERS                                         
*                                                                               
VALOPT   XC    FILTERS,FILTERS                                                  
         LA    R2,BASOPTH                                                       
         CLI   5(R2),0                                                          
         BE    VALOPTX2                                                         
         MVI   ERRNUM,EIIF                                                      
         GOTO1 SCANNER,DMCB,(R2),(7,LINES)                                      
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         MVC   NLINES,4(R1)        SAVE NUMBER OF LINES INPUT                   
         MVI   FNDX,1                                                           
         LA    R6,LINES                                                         
VALOPT2  CLI   0(R6),0                                                          
         BE    OPTERR                                                           
         CLI   1(R6),0                                                          
         BNE   VALOPT4                                                          
         MVI   ERRNUM,EIIF                                                      
         CLC   12(6,R6),=C'SWITCH'                                              
         BE    VALOPT4                                                          
         CLC   12(5,R6),=C'DDATE'                                               
         BE    VALOPT4                                                          
         CLC   12(4,R6),=C'OVER'                                                
         BE    VALOPT4                                                          
         CLI   0(R6),12                                                         
         BH    OPTERR                                                           
         OC    SRCEFILT,SRCEFILT   FILTER ALREADY INPUT                         
         BNZ   OPTERR                                                           
         ZIC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRCEFILT+1(0),12(R6)                                             
         STC   R1,SRCEFILT         SAVE FILTER AND LENGTH                       
         B     VALOPTX                                                          
*                                  VALIDATE KEYWORD OPTIONS/FILTERS             
VALOPT4  ZIC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
         LA    RE,OPTTAB                                                        
         MVI   ERRNUM,EIIF                                                      
VALOPT6  CLI   0(RE),0                                                          
         BE    OPTERR                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),12(R6)                                                   
         BE    *+12                                                             
         LA    RE,L'OPTTAB(RE)                                                  
         B     VALOPT6                                                          
         L     RE,8(RE)            A(VALIDATION ROUTINE)                        
         A     RE,MYRELO                                                        
         BR    RE                  GO TO IT                                     
OPTERR   XC    CONHEAD,CONHEAD                                                  
         MVC   MSG,SPACES                                                       
         GOTO1 GETMSG,DMCB+12,(ERRNUM,MSG),(FNDX,DMCB),0                        
         B     EXIT                                                             
         EJECT                                                                  
*                                  VALIDATE BILL NUMBER FILTER                  
VALBILL  MVI   ERRNUM,EDIF                                                      
         OC    BILLFILT,BILLFILT                                                
         BNZ   OPTERR                                                           
         MVI   ERRNUM,EFTL                                                      
         CLI   1(R6),6                                                          
         BH    OPTERR                                                           
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BILLFILT+1(0),22(R6)                                             
         STC   R1,BILLFILT                                                      
         B     VALOPTX                                                          
*                                  VALIDATE START BILL NUMBER                   
VALSBIL  MVI   ERRNUM,EDIF                                                      
         OC    SBILFILT,SBILFILT                                                
         BNZ   OPTERR                                                           
         MVI   ERRNUM,EFTL                                                      
         CLI   1(R6),6                                                          
         BH    OPTERR                                                           
         MVC   SBILFILT,22(R6)                                                  
         B     VALOPTX                                                          
*                                  SET SWITCH                                   
VALSWIT  MVI   SWITCH,1                                                         
         B     VALOPTX                                                          
*                                  VALIDATE INCOME A/C FOR GROSS OPTN           
VALINC   MVI   ERRNUM,EDIF                                                      
         OC    INCOMAC,INCOMAC                                                  
         BNZ   OPTERR                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SI'     HARD CODED LEDGER                            
         ZIC   R3,1(R6)                                                         
         LA    R4,22(R6)                                                        
         LA    R5,KEY+3                                                         
         CLI   0(R4),C'*'          CAN BE *ULACC                                
         BNE   VALINC2                                                          
         MVI   ERRNUM,EFTS                                                      
         CLI   1(R6),4                                                          
         BL    OPTERR                                                           
         BCTR  R3,0                                                             
         LA    R4,1(R4)                                                         
         LA    R5,KEY+1                                                         
VALINC2  BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R4)                                                    
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,EIAC                                                      
         TM    ACCTSTAT,X'80'      CHECK IF A/C IS OK TO POST TO                
         BZ    OPTERR                                                           
         MVI   ERRNUM,EAIL                                                      
         TM    ACCTSTAT,X'10'                                                   
         BO    OPTERR                                                           
         MVC   INCOMAC,ACCTNUM                                                  
         MVC   INCOMNM,ACCTNAME                                                 
         B     VALOPTX                                                          
*                                  VALIDATE START/END DATE FILTER               
VALDATE  MVI   ERRNUM,EIDF                                                      
         GOTO1 DATVAL,DMCB,22(R6),WORK                                          
         OC    0(4,R1),0(R1)                                                    
         BZ    OPTERR                                                           
         LA    R3,SDATFILT                                                      
         CLI   12(R6),C'S'                                                      
         BE    *+8                                                              
         LA    R3,EDATFILT                                                      
         MVI   ERRNUM,EDIF                                                      
         OC    0(3,R3),0(R3)                                                    
         BNZ   OPTERR                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,0(R3))                                   
         MVI   ERRNUM,EIDF         YEAR MUST BE GR 1960                         
         CLI   0(R3),X'40'                                                      
         BNH   OPTERR                                                           
         GOTO1 DATECHK,DMCB,0(R3)                                               
         CLI   DMCB,X'FF'                                                       
         BE    OPTERR                                                           
         B     VALOPTX                                                          
         SPACE 1                                                                
VALDDATE MVI   ERRNUM,EIIF                                                      
         TM    OPT1,DDATE                                                       
         BO    OPTERR                                                           
         CLI   1(R6),0                                                          
         BNE   OPTERR                                                           
         OI    OPT1,DDATE                                                       
         B     VALOPTX                                                          
         SPACE 1                                                                
VALOVER  MVI   ERRNUM,EIIF                                                      
         TM    OPT1,OVER                                                        
         BO    OPTERR                                                           
         CLI   1(R6),0                                                          
         BNE   OPTERR                                                           
         OI    OPT1,OVER                                                        
         B     VALOPTX                                                          
*                                                                               
VALOPTX  ZIC   R1,FNDX             BUMP FIELD INDEX                             
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R6,32(R6)           AND BLOCK POINTER                            
         CLC   FNDX,NLINES                                                      
         BNH   VALOPT2                                                          
         MVI   FNDX,0                                                           
VALOPTX2 OC    EDATFILT,EDATFILT   IF END DATE NOT SET - SET HIGH               
         BNZ   *+10                                                             
         MVC   EDATFILT,=3X'FF'                                                 
         MVI   ERRNUM,EDNC                                                      
         CLC   SDATFILT,EDATFILT                                                
         BH    OPTERR                                                           
         CLI   MODE,0                                                           
         BNE   VALCASH                                                          
         B     VALCRAC                                                          
         EJECT                                                                  
*              VALIDATE CREDIT ACCOUNT                                          
*                                                                               
VALCRAC  LA    R7,COMPEL                                                        
         USING ACCOMPD,R7          R7=A(COMPANY ELEMENT)                        
         LA    R2,CALCRACH                                                      
         BAS   RE,ANY                                                           
         MVI   ERRNUM,EIIF                                                      
         GOTO1 SCANNER,DMCB,(R2),(7,LINES)                                      
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         MVC   NLINES,4(R1)                                                     
         MVI   FNDX,1                                                           
         LA    R6,LINES            R6=A(SCAN BLOCK)                             
         LA    R5,CRACTAB          R5=A(OUTPUT TABLE)                           
         ST    R5,ACRAC                                                         
VALCRAC2 MVI   ERRNUM,EIIF                                                      
         CLI   0(R6),0                                                          
         BE    OPTERR                                                           
         CLI   1(R6),0                                                          
         BNE   OPTERR                                                           
         MVC   KEY,SPACES          BUILD ACCOUNT KEY                            
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),ACMPRECV   RECEIVABLES                                  
         XC    CLIPROF,CLIPROF                                                  
         XC    PRODPROF,PRODPROF                                                
         ZIC   R3,0(R6)                                                         
         LA    R4,12(R6)                                                        
         LA    R1,KEY+3                                                         
         CLI   0(R4),C'*'          CAN BE *ULACC                                
         BNE   VALCRAC4                                                         
         MVI   ERRNUM,EFTS                                                      
         CLI   0(R6),4                                                          
         BL    OPTERR                                                           
         MVI   ERRNUM,EIPL                                                      
         CLI   1(R4),C'S'          CHECK FOR VALID UNIT                         
         BNE   OPTERR                                                           
         BCTR  R3,0                                                             
         LA    R4,1(R4)                                                         
         LA    R1,KEY+1                                                         
VALCRAC4 BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R4)                                                    
VALCRAC5 BAS   RE,GETACC                                                        
         MVI   ERRNUM,EIAC                                                      
         TM    ACCTSTAT,X'80'      SEE IF ACCOUNT OK TO POST TO                 
         BZ    OPTERR                                                           
         MVI   ERRNUM,EAIL                                                      
         TM    ACCTSTAT,X'10'      SEE IF ACCOUNT IS LOCKED                     
         BO    OPTERR                                                           
         CLI   FNDX,1                                                           
         BNE   *+16                                                             
         MVC   CRAC,ACCTNUM        SAVE CODE/NAME OF FIRST ACCOUNT              
         MVC   CRNM,ACCTNAME                                                    
         MVC   0(15,R5),KEY        SAVE KEY IN CRACTAB                          
         ZIC   R1,FNDX             BUMP TO NEXTS                                
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R6,32(R6)                                                        
         LA    R5,15(R5)                                                        
         CLC   FNDX,NLINES                                                      
         BNH   VALCRAC2                                                         
         MVI   0(R5),X'FF'         SET END OF CRACTAB                           
         MVI   FNDX,0                                                           
         B     VALBANK                                                          
         EJECT                                                                  
*              VALIDATE DEBIT ACCOUNT (BANK)                                    
*                                                                               
VALBANK  MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),ACMPBANK                                                
         LA    R2,CALBANKH                                                      
         BAS   RE,ANY                                                           
         ZIC   R3,5(R2)                                                         
         LA    R4,8(R2)                                                         
         LA    R5,KEY+3                                                         
         CLI   8(R2),C'*'          CAN BE *ULACC                                
         BNE   VALBANK2                                                         
         MVI   ERRNUM,EFTS                                                      
         CLI   5(R2),4                                                          
         BL    ERROR                                                            
         MVI   ERRNUM,EIPL                                                      
*&&UK                                                                           
         CLC   1(2,R4),=C'SQ'      CHECK UNIT/LEDGER                            
*&&                                                                             
*&&US                                                                           
         CLC   1(2,R4),=C'GB'                                                   
*&&                                                                             
         BE    VALBANK1                                                         
         CLC   1(2,R4),=C'SB'                                                   
         BE    VALBANK1                                                         
         CLC   1(2,R4),=C'GP'                                                   
         BE    VALBANK1                                                         
         CLC   1(2,R4),=C'SE'                                                   
         BNE   ERROR                                                            
VALBANK1 BCTR  R3,0                                                             
         LA    R4,9(R2)                                                         
         LA    R5,KEY+1                                                         
VALBANK2 BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R4)       MOVE TO KEY                                  
         BAS   RE,GETACC                                                        
         MVC   DRAC,ACCTNUM        SAVE ACCOUNT CODE                            
         MVI   ERRNUM,EIAC                                                      
         TM    ACCTSTAT,X'80'      SEE IF OK TO POST                            
         BZ    ERROR                                                            
         MVI   ERRNUM,EAIL                                                      
         TM    ACCTSTAT,X'10'                                                   
         BO    ERROR                                                            
         B     VALCHK                                                           
         EJECT                                                                  
*              VALIDATE CHEQUE NUMBER/DATE                                      
*                                                                               
VALCHK   LA    R2,CALCNUMH                                                      
         BAS   RE,ANY                                                           
*&&US                                                                           
         MVI   ERRNUM,EFTS                                                      
         CLI   5(R2),6                                                          
         BL    ERROR                                                            
         MVI   ERRNUM,EFNN                                                      
         TM    4(R2),X'08'         MUST BE 6 CHRS NUMERIC                       
         BZ    ERROR                                                            
*&&                                                                             
         MVC   CHKNUM,SPACES                                                    
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CHKNUM(0),8(R2)                                                  
*                                                                               
VALCHKD  LA    R2,CALCDATH                                                      
         BAS   RE,ANY                                                           
         MVI   ERRNUM,EIDF                                                      
         GOTO1 DATVAL,DMCB,8(R2),WORK                                           
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKDATE)                                 
         GOTO1 DATECHK,DMCB,CHKDATE                                             
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
*                                  VALIDATE DEPOSIT DATE                        
VALDEPD  LA    R2,CALDDATH                                                      
         BAS   RE,ANY                                                           
         MVI   ERRNUM,EIDF                                                      
         GOTO1 DATVAL,DMCB,8(R2),WORK                                           
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,DEPDATE)                                 
         GOTO1 DATECHK,DMCB,DEPDATE                                             
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
*                                  OVERLAY DISPLAY SCREEN                       
         GOTO1 NEWSCRN,DMCB,(X'DF',0)                                           
         XC    LKEY,LKEY           SET KEY TO FIRST                             
         ZAP   LTOTAL,=P'0'        CLEAR TOTAL FIELDS                           
         ZAP   LTOTALT,=P'0'                                                    
         B     DISPLAY                                                          
         EJECT                                                                  
*              DISPLAY UP TO 25 ENTRIES ON SCREEN                               
*                                                                               
DISPLAY  TWAXC CALL1H,CALCRNMH,PROT=Y                                           
         MVC   CALCRNM,SPACES      CREDIT ACCOUNT CODE/NAME                     
         MVC   CALCRNM(12),CRAC+3                                               
         LA    R1,CALCRNM+11                                                    
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(L'CRNM,R1),CRNM                                                
         XC    LASTLEFT,LASTLEFT                                                
         XC    LASTRGHT,LASTRGHT                                                
         MVI   THISSIDE,0                                                       
         XC    DISCTAB,DISCTAB                                                  
         LA    R2,DISCTAB          R2=A(CASH DISCOUNT TABLE)                    
         LA    R3,CALL1H           R3=A(DISPLAY LINE)                           
         USING LINED,R3                                                         
         LA    R4,KEY              R4=A(ACCOUNT KEY)                            
         USING ACKEYD,R4                                                        
         LA    R5,ACRECORD                                                      
         USING TRANSD,R5           R5=A(TRANSACTION ELEMENT)                    
         MVI   COUNT,0             CLEAR LINES OUTPUT                           
         XC    LASTS,LASTS                                                      
         ZAP   LAMOUNT,=P'0'                                                    
         ZAP   0(6,R2),=P'0'                                                    
         MVC   KEY,LKEY                                                         
         CLI   KEY,0                                                            
         BNE   DISP2                                                            
         MVI   WORK,X'F0'          BUILD FIRST KEY                              
         CLC   CRAC+1(2),=C'SR'                                                 
         BE    *+8                                                              
         MVI   WORK,X'80'         IF NOT SR CANT BE SURE OF CONTRA              
         BAS   RE,KEYBUILD                                                      
DISP2    LR    R0,R2               ENSURE FADR HAS A TWA ADDRESS                
         LA    R2,CALL1H           IN CASE OF DELETES                           
         BAS   RE,HIGH                                                          
         B     DISP6                                                            
DISP4    LR    R0,R2                                                            
         LA    R2,CALL1H                                                        
         BAS   RE,SEQ                                                           
DISP6    LR    R2,R0               RESET R2 - DISCTAB                           
         MVC   LKEY,KEY                                                         
         CLC   ACKEYACC,CRAC       CHANGE OF ACCOUNT                            
         BE    DISP8                                                            
         BAS   RE,FORMAT                                                        
         MVI   LKEY,X'FF'                                                       
         B     ALLEXIT                                                          
DISP8    OC    SRCEFILT,SRCEFILT   SUB-ACCOUNT FILTER                           
         BZ    DISP10                                                           
         ZIC   R1,SRCEFILT         YES - THIS ONE OK?                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACKEYCON+3(0),SRCEFILT+1                                         
         BE    DISP10                                                           
         BAS   RE,FORMAT                                                        
         MVI   LKEY,X'FF'                                                       
         B     ALLEXIT                                                          
DISP10   CLC   ACKEYCON(3),SPACES  CHECK FOR VALID SUB-ACCOUNT                  
         B     DISP11                                                           
         BE    DISP11                                                           
         BAS   RE,FORMAT                                                        
         MVI   WORK,X'07'                                                       
         BAS   RE,KEYBUILD                                                      
         B     DISP2                                                            
DISP11   CLI   ACRECORD,X'44'      SUB-ACCOUNT RECORD TEST                      
         BE    DISP12                                                           
         BAS   RE,FORMAT                                                        
         MVI   WORK,X'30'                                                       
         BAS   RE,KEYBUILD                                                      
         B     DISP2                                                            
DISP12   CLC   TRNSDATE,EDATFILT   END DATE TEST                                
         BNH   DISP14                                                           
         BAS   RE,FORMAT                                                        
         MVI   WORK,X'34'                                                       
         BAS   RE,KEYBUILD                                                      
         B     DISP2                                                            
DISP14   OC    BILLFILT,BILLFILT   BILL NUMBER TEST                             
         BZ    DISP15                                                           
         ZIC   R1,BILLFILT                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TRNSREF(0),BILLFILT+1                                            
         BE    DISP16                                                           
         BAS   RE,FORMAT                                                        
         B     DISP4                                                            
DISP15   OC    SBILFILT,SBILFILT   START BILL FILTER                            
         BZ    DISP16                                                           
         CLC   TRNSREF,SBILFILT                                                 
         BNL   DISP16                                                           
         BAS   RE,FORMAT                                                        
         MVI   WORK,X'10'                                                       
         BAS   RE,KEYBUILD                                                      
         B     DISP2                                                            
DISP16   OC    ACDTPEEL,ACDTPEEL   CHECK FOR DELETED TRANSACTION                
         BNZ   DISP4                                                            
         OC    LASTS,LASTS         FIRST TIME TEST                              
         BZ    DISP20                                                           
         CLC   ACKEYCON+3(0),LSOURCE                                            
         BNE   DISP18                                                           
         CLC   TRNSREF,LREF                                                     
         BNE   DISP18                                                           
         CLC   ACKEYCON+3(6),=C'TALENT' SPECIAL FOR TALENT                      
         BNE   *+14                                                             
         CLC   TRNSANAL,6(R2)      COMPARE THIS OFFICE TO LAST                  
         BNE   DISP18                                                           
         TM    OPT1,DDATE          OPTION TO SHOW DIFFERENT                     
         BZ    DISP22              DATES FOR SAME INVOICE NUMBER                
         CLC   LDATE,TRNSDATE                                                   
         BNE   DISP18                                                           
         B     DISP22                                                           
DISP18   BAS   RE,FORMAT                                                        
DISP20   MVC   LSOURCE,ACKEYCON+3  SAVE THIS TIME VALUES                        
         MVC   FIRST3,ACKEYCON                                                  
         MVC   LREF,TRNSREF                                                     
         MVC   LDATE,TRNSDATE                                                   
         ZAP   LAMOUNT,=P'0'                                                    
         ZAP   0(6,R2),=P'0'                                                    
         MVC   6(2,R2),SPACES                                                   
DISP22   ZAP   DUB,TRNSAMNT        SAVE NET AMOUNT AND SET C/D VALUE            
         ZAP   DUB2,=P'0'                                                       
         MVC   6(2,R2),TRNSANAL    *** SEE IF THIS WORKS ***                    
         TM    TRNSSTAT,X'80'      IF THIS TRANSACTION IS A DEBIT               
         BZ    DISP32                                                           
         OC    INCOMAC,INCOMAC     AND WE ARE RUNNING WITH GROSS OPTN           
         BZ    DISP34                                                           
         ZIC   R1,1(R5)            LOOK FOR EXTRA CASH ELEMENT                  
         AR    R1,R5                                                            
         SR    RF,RF                                                            
DISP24   CLI   0(R1),0                                                          
         BE    DISP34                                                           
         CLI   0(R1),X'50'                                                      
         BE    DISP28                                                           
DISP26   IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     DISP24                                                           
DISP28   DS    0H                                                               
         USING TRCASHD,R1                                                       
         CLI   TRCSTYPE,C'D'                                                    
         BNE   DISP26                                                           
         CP    TRCSAMNT,=P'0'                                                   
         BE    DISP26                                                           
         ZAP   DUB2,TRCSAMNT       SAVE C/D FROM EXTRA CASH ELEMENT             
         DROP  R1                                                               
         B     DISP34                                                           
DISP32   ZAP   0(6,R2),=P'0'       CLEAR C/D ON DEBIT (DISCOUNT ALREADY         
         ZAP   DUB2,=P'0'                                                       
         MP    DUB,=P'-1'                                                       
DISP34   AP    LAMOUNT,DUB                                                      
         AP    0(6,R2),DUB2                                                     
         B     DISP4                                                            
         EJECT                                                                  
*              DISPLAY OVERPAYMENT SCREEN                                       
         SPACE 1                                                                
DISPOVER TWAXC CALOVR1H,CALOVCRH,PROT=Y                                         
         MVC   CALOVCR,SPACES      CREDIT ACCOUNT CODE/NAME                     
         MVC   CALOVCR(12),CRAC+3                                               
         LA    R1,CALOVCR+11                                                    
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(L'CRNM,R1),CRNM                                                
         SR    R1,R1                                                            
         LA    R2,CALOVR1H                                                      
         MVC   SRCETBL(3*MAXCOUNT),SPACES  CLEAR TABLE OF FIRST 3               
         LA    R3,OVRLIST                                                       
         XC    DISCTAB,DISCTAB                                                  
         LA    R6,DISCTAB                                                       
DISPOV1  MVC   8(12,R2),0(R3)      PUT OUT CONTRA NAMES                         
         ZAP   0(6,R6),=P'0'                                                    
         MVC   6(2,R6),SPACES                                                   
         LA    R6,8(R6)                                                         
         LA    R1,1(R1)                                                         
         LA    R3,12(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BE    DISPOV3                                                          
DISPOV2  ZIC   R5,0(R2)                                                         
         AR    R2,R5                                                            
         TM    1(R2),X'20'                                                      
         BO    DISPOV1             PROTECTED                                    
         B     DISPOV2                                                          
         SPACE 1                                                                
DISPOV3  STC   R1,COUNT                                                         
         BAS   RE,TOTLNE                                                        
         B     ALLEXIT                                                          
         EJECT                                                                  
*              BUILD A VIRGIN KEY                                               
*                                                                               
KEYBUILD NTR1                                                                   
         SR    RE,RE                                                            
         TM    WORK,X'80'          X'80' - CLEAR KEY SET ACCOUNT                
         BZ    KEYB2                                                            
         MVC   KEY,SPACES                                                       
         MVC   ACKEYACC,CRAC                                                    
KEYB2    TM    WORK,X'40'          X'40' - SET SUB-ACCOUNT                      
         BZ    KEYB4                                                            
         MVC   ACKEYCON,SPACES                                                  
         MVI   ACKEYCON+5,X'41'                                                 
         OC    SRCEFILT,SRCEFILT                                                
         BZ    KEYB4                                                            
         IC    RE,SRCEFILT                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ACKEYCON+3(0),SRCEFILT+1                                         
KEYB4    TM    WORK,X'20'          X'20' - SET START DATE                       
         BZ    KEYB6                                                            
         MVC   ACKEYDTE,SDATFILT                                                
         OC    ACKEYDTE,ACKEYDTE                                                
         BNZ   KEYB6                                                            
         MVI   ACKEYDTE,X'60'      (GR X'40' IF NOT SPEC)                       
KEYB6    TM    WORK,X'10'          X'10' - SET BILL NUMBER                      
         BZ    KEYB8                                                            
         MVC   ACKEYREF,SPACES                                                  
         OC    SBILFILT,SBILFILT                                                
         BZ    *+10                                                             
         MVC   ACKEYREF,SBILFILT                                                
         OC    BILLFILT,BILLFILT                                                
         BZ    KEYB8                                                            
         IC    RE,BILLFILT                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ACKEYREF(0),BILLFILT+1                                           
KEYB8    TM    WORK,X'04'          X'04' - BUMP SUB-ACCOUNT                     
         BZ    KEYB10                                                           
         IC    RE,ACKEYCON+14                                                   
         LA    RE,1(RE)                                                         
         STC   RE,ACKEYCON+14                                                   
KEYB10   TM    WORK,X'02'          X'02' - BUMP DATE                            
         BZ    KEYB12                                                           
         IC    RE,ACKEYDTE+2                                                    
         LA    RE,1(RE)                                                         
         STC   RE,ACKEYDTE+2                                                    
KEYB12   TM    WORK,X'01'          X'01' - BUMP BILL NUMBER                     
         BZ    KEYBX                                                            
         IC    RE,ACKEYREF+5                                                    
         LA    RE,1(RE)                                                         
         STC   RE,ACKEYREF+5                                                    
KEYBX    MVI   ACKEYSBR,0                                                       
         XIT1                                                                   
         EJECT                                                                  
*              FORMAT 1 ENTRY INTO DISPLAY LINE                                 
*                                                                               
FORMAT   NTR1                                                                   
         CP    LAMOUNT,=P'0'       IGNORE IF ZERO MONEY                         
         BE    FORM2                                                            
         ZIC   R1,COUNT                                                         
         MH    R1,=H'3'                                                         
         LA    R1,SRCETBL(R1)                                                   
         MVC   0(3,R1),FIRST3      SAVE FIRST 3 OF CONTRA                       
         LA    R1,LASTLEFT         SEE IF NAME ABOVE                            
         CLI   THISSIDE,0                                                       
         BE    *+8                                                              
         LA    R1,LASTRGHT                                                      
         CLC   0(12,R1),LSOURCE                                                 
         BE    *+16                                                             
         MVC   LINSRCE,LSOURCE     SOURCE NAME                                  
         MVC   0(12,R1),LSOURCE                                                 
         XI    THISSIDE,X'FF'      SWITCH SIDES                                 
         MVC   LINBILL,LREF        BILL NUMBER                                  
         GOTO1 DATCON,DMCB,(1,LDATE),(0,WORK)                                   
         MVC   LINDATE(2),WORK+4   DATE - UK FORMAT DDMMMYY                     
         PACK  DUB,WORK+2(2)                                                    
         CVB   R1,DUB                                                           
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   LINDATE+2(3),0(R1)                                               
         MVC   LINDATE+5(2),WORK                                                
         ZAP   DUB2,LAMOUNT                                                     
         AP    DUB2,0(6,R2)                                                     
         EDIT  (P8,DUB2),(11,LINAMNT),2,MINUS=YES,ALIGN=LEFT                    
*&&US                                                                           
         CLI   SWITCH,0                                                         
         BE    *+8                                                              
         MVI   LINSWIT,C'+'        US SCREEN PROBLEM                            
*&&                                                                             
         LA    R3,LINNEXT(R3)      BUMP TO NEXT DISPLAY LINE                    
         LA    R2,8(R2)                                                         
         ZIC   R1,COUNT            BUMP DISPLAY COUNT                           
         LA    R1,1(R1)                                                         
         STC   R1,COUNT                                                         
         CLI   COUNT,MAXCOUNT      SCREEN FULL                                  
         BNE   FORM2                                                            
         L     RD,SAVERD                                                        
         LM    RE,RC,12(RD)                                                     
         B     ALLEXIT                                                          
FORM2    XC    LASTS,LASTS                                                      
         ZAP   LAMOUNT,=P'0'                                                    
         ZAP   0(6,R2),=P'0'                                                    
         MVC   6(2,R2),SPACES                                                   
         XIT1  REGS=(R2,R3)                                                     
         DROP  R3                                                               
         EJECT                                                                  
*              VALIDATE INPUT DATA                                              
*                                                                               
VALCASH  LA    R2,CONACTH          CHECK ACTION SEQUENCE                        
         MVI   ERRNUM,EIAS                                                      
         CLI   MODE,X'FF'                                                       
         BE    ERROR                                                            
         SR    R6,R6                                                            
         MVC   KEY,SPACES          READ DR/CR ACCOUNTS AND SAVE NAMES           
         LA    R1,CRACTAB          FIND FIRST N/Z ENTRY IN CRACTAB              
VALC1    OC    0(15,R1),0(R1)                                                   
         BNZ   *+12                                                             
         LA    R1,15(R1)                                                        
         B     VALC1                                                            
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(15),0(R1)                                                    
         ST    R1,ACRAC                                                         
         BAS   RE,GETACC                                                        
         MVC   CRNM,ACCTNAME                                                    
         MVC   KEY(15),DRAC                                                     
         BAS   RE,GETACC                                                        
         MVC   DRNM,ACCTNAME                                                    
         CLI   SCREEN,OVSCRN                                                    
         BE    VALOVCSH                                                         
*                                  VALIDATE CASH FIELDS                         
         LA    R5,CALL1H                                                        
         USING LINED,R5            R5=A(TWA LINE)                               
         XC    CASHTAB,CASHTAB                                                  
         ZAP   LTOTAL,=P'0'                                                     
         LA    R3,CASHTAB          R3=A(CASHTAB ENTRY)                          
         LA    R6,DISCTAB                                                       
         ZIC   R4,COUNT            R4=NUMBER OF LINES DISPLAYED                 
VALC2    LA    R2,LINAMNTH                                                      
         MVI   ERRNUM,EMIF                                                      
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         ZIC   R1,5(R2)                                                         
         STC   R1,WORK                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),8(R2)     MOVE FIELD TO WORK                           
         OI    WORK+1,X'40'                                                     
         CLI   SWITCH,0            NORMAL MODE                                  
         BNE   VALC4                                                            
         CLI   WORK+1,C' '         UNCHANGED - MAKE A POSTING                   
         BE    VALC6                                                            
         CLI   WORK+1,C'N'         'N' - DON'T MAKE A POSTING                   
         BNE   VALC6                                                            
         B     VALC8               CHANGED - MAKE A POSTING                     
VALC4    MVI   ERRNUM,EIIF         SWITCHED MODE                                
*&&UK*&& CLI   WORK+1,C' '                                                      
*&&US*&& CLI   WORK+1,C'+'                                                      
         BE    VALC8               UNCHANGED - DON'T MAKE A POSTING             
         CLI   WORK+1,C'Y'         'Y' OR AMOUNT - MAKE A POSTING               
         BNE   VALC6                                                            
         MVI   WORK+1,C' '                                                      
VALC6    ZIC   R0,WORK                                                          
         GOTO1 =V(AMTVAL),DMCB,(2,WORK+1),(R0),MAXAMT,RR=RB                     
         MVI   ERRNUM,EIIA                                                      
         CLI   DMCB,X'FF'          CHECK FOR VALID NUMERIC                      
         BE    ERROR                                                            
         L     R1,DMCB+4                                                        
         ZAP   DUB,0(8,R1)                                                      
         OC    DUB(2),DUB                                                       
         BNZ   ERROR               MORE THAN SIX BYTES                          
         AP    LTOTAL,DUB          ADD AMOUNT TO TOTAL FOR THIS SCREEN          
         ZAP   0(6,R3),DUB                                                      
         MVI   ERRNUM,ELCD                                                      
VALC8    LA    R5,LINNEXT(R5)      BUMP TO NEXTS                                
         LA    R3,6(R3)                                                         
         LA    R6,8(R6)                                                         
         BCT   R4,VALC2                                                         
         AP    LTOTALT,LTOTAL     ADD AMOUNT TO TOTAL SO FAR                    
         B     POST                                                             
         DROP  R5                                                               
         EJECT                                                                  
*              VALIDATE OVERPAYMENT SCREEN                                      
         SPACE 1                                                                
VALOVCSH XC    CASHTAB,CASHTAB                                                  
         ZAP   LTOTAL,=P'0'                                                     
         XC    WKLINE,WKLINE                                                    
         ZAP   LTOTAL,=P'0'                                                     
         LA    R6,OVRLIST                                                       
         LA    R2,CALOVR1H                                                      
         LA    R3,CASHTAB                                                       
         ZIC   R4,COUNT                                                         
         LA    R5,WKLINE                                                        
         LA    R7,DISCTAB                                                       
         SPACE 1                                                                
         USING LINED,R5                                                         
VALOVC10 MVC   LINSRCE,0(R6)                                                    
         ZIC   R1,0(R2)            GET BILL NUMBER                              
         AR    R2,R1                                                            
         CLI   5(R2),0                                                          
         BE    VALOVC30                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LINBILL(0),8(R2)                                                 
         OC    LINBILL,SPACES                                                   
         SPACE 1                                                                
VALOVC15 ZIC   R1,0(R2)            DATE                                         
         AR    R2,R1                                                            
         BAS   RE,ANY                                                           
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         MVI   ERRNUM,EIDF                                                      
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         MVC   LINDATE(2),WORK+4   DATE - UK FORMAT DDMMMYY                     
         PACK  DUB,WORK+2(2)                                                    
         CVB   R1,DUB                                                           
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   LINDATE+2(3),0(R1)                                               
         MVC   LINDATE+5(2),WORK                                                
         SPACE 1                                                                
VALOVC20 ZIC   R1,0(R2)            AMOUNT                                       
         AR    R2,R1                                                            
         BAS   RE,ANY                                                           
         ZIC   R0,5(R2)                                                         
         GOTO1 =V(AMTVAL),DMCB,(2,8(R2)),(R0),MAXAMT,RR=RB                      
         MVI   ERRNUM,EIIA                                                      
         CLI   DMCB,X'FF'          CHECK FOR VALID NUMERIC                      
         BE    ERROR                                                            
         L     R1,DMCB+4                                                        
         ZAP   DUB,0(8,R1)                                                      
         OC    DUB(2),DUB                                                       
         BNZ   ERROR               MORE THAN 6 BYTES                            
         AP    LTOTAL,DUB          ADD AMOUNT TO TOTAL FOR THIS SCREEN          
         ZAP   0(6,R3),DUB                                                      
         ZIC   R1,0(R2)            OFFICE                                       
         AR    R2,R1                                                            
         OC    8(2,R2),SPACES                                                   
         TM    COMPSTAT,X'20'      OFFICE BASED                                 
         BNO   *+8                                                              
         BAS   RE,ANY              IF SO ANALYSIS COMPULSORY                    
         CLI   5(R2),0                                                          
         BE    VALOVC21                                                         
         GOTO1 AVALOFFC,DMCB,(X'80',8(R2))                                      
         CLI   ERRNUM,OK                                                        
         BNE   ERROR                                                            
*                                                                               
VALOVC21 MVC   6(2,R7),8(R2)       ANALYSIS CODE TO TAB. ENTRY                  
*&&UK                                                                           
         CLI   TWAACCS,C'*'        BUT TEST IF OFFICE LOGON                     
         BNE   VALOVC25                                                         
         CLC   8(1,R2),TWAACCS+1   AND IF SO THAT INPUT IS CORRECT              
         BE    VALOVC25                                                         
         MVI   ERRNUM,SECLOCK                                                   
         B     ERROR                                                            
*&&                                                                             
VALOVC25 LA    R5,LINNEXT(R5)                                                   
         XC    0(L'WKLINE,R5),0(R5)                                             
         LA    R3,6(R3)                                                         
         LA    R6,12(R6)                                                        
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               START OF NEXT LINE                           
         LA    R7,8(R7)            NEXT DISCTAB ENTRY                           
         BCT   R4,VALOVC10                                                      
         AP    LTOTALT,LTOTAL      ADD AMOUNT TO TOTAL SO FAR                   
         B     POST                                                             
         DROP  R5                                                               
         SPACE 1                                                                
VALOVC30 MVI   ERRNUM,EIIF                                                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         B     VALOVC25                                                         
         EJECT                                                                  
*              GENERATE POSTINGS                                                
*                                                                               
POST     LA    R2,CALL1H           R2=A(TWA LINE)                               
         CLI   SCREEN,OVSCRN                                                    
         BNE   *+8                                                              
         LA    R2,WKLINE                                                        
         USING LINED,R2                                                         
         LA    R3,CASHTAB          R3=A(CASH TAB ENTRY)                         
         LA    R6,DISCTAB                                                       
         XC    LASTLEFT,LASTLEFT                                                
         ZIC   R4,COUNT                                                         
         LA    R5,SRCETBL          TABLE OF FIRST 3 OF CONTRA                   
POST2    OC    LINSRCE,LINSRCE     SAME AS LAST SOURCE                          
         BZ    *+10                                                             
         MVC   LASTLEFT,LINSRCE                                                 
         OC    0(6,R3),0(R3)       ZEROES - IGNORE                              
         BZ    POST4                                                            
         CP    0(6,R3),=P'0'       IGNORE ZERO POSTINGS                         
         BE    POST4                                                            
         MVC   FIRST3,0(R5)        FIRST 3 OF CONTRA                            
         BAS   RE,BLDTRNS          BUILD POSTINGS                               
         BAS   RE,PUTDAY           ADD TO ACCDAY                                
         XC    WORK,WORK                                                        
         MVC   WORK(6),LINBILL                                                  
         L     R1,DMCB+8                                                        
         MVC   WORK+10(4),0(R1)                                                 
         ZAP   TRANSAMT,0(6,R3)                                                 
         BAS   RE,ADTWA1           AND TWA1                                     
POST4    LA    R3,6(R3)                                                         
         LA    R6,8(R6)                                                         
         LA    R5,3(R5)                                                         
         LA    R2,LINNEXT(R2)                                                   
         BCT   R4,POST2                                                         
         BAS   RE,TOTLNE                                                        
         CLI   LKEY,X'FF'                                                       
         BNE   DISPLAY                                                          
         MVI   MODE,X'FF'                                                       
         B     ALLEXIT                                                          
         EJECT                                                                  
*              BUILD A POSTING RECORD                                           
*                                                                               
BLDTRNS  NTR1                                                                   
         LA    R5,IOAREA+2         R5=A(CURRENT ELEMENT)                        
         USING DLDESCD,R5          BUILD X'64' ELEMENT                          
         XC    DLDSEL(100),DLDSEL                                               
         MVI   DLDSEL,X'64'                                                     
         MVI   DLDSLEN,76                                                       
         MVC   WORK(2),LINDATE+5   CONVERT DDMMMYY TO YMD (PWOS)                
         MVC   WORK+4(2),LINDATE                                                
         LA    R1,MONTHS                                                        
         LA    RE,1                                                             
BUILDT2  CLC   LINDATE+2(3),0(R1)                                               
         BE    *+16                                                             
         LA    R1,3(R1)                                                         
         LA    RE,1(RE)                                                         
         B     BUILDT2                                                          
         CVD   RE,DUB                                                           
         UNPK  WORK+2(2),DUB                                                    
         OI    WORK+3,X'F0'                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(1,DLDSDATE)                                
         MVC   DLDSREF,LINBILL                                                  
         MVC   DLDSNARR(57),SPACES                                              
*&&UK                                                                           
         MVC   DLDSNARR(13),=C'CHEQUE NUMBER'                                   
         MVC   DLDSNARR+14(L'CHKNUM),CHKNUM                                     
         MVC   DLDSNARR+21(5),=C'DATED'                                         
         GOTO1 (RF),(R1),(1,CHKDATE),(8,DLDSNARR+27)                            
         MVC   DLDSNARR+35(12),=C'DEPOSITED ON'                                 
         GOTO1 (RF),(R1),(1,DEPDATE),(8,DLDSNARR+48)                            
*&&                                                                             
*&&US                                                                           
         MVC   DLDSNARR(12),=C'CHECK NUMBER'                                    
         MVC   DLDSNARR+13(L'CHKNUM),CHKNUM                                     
         MVC   DLDSNARR+20(5),=C'DATED'                                         
         GOTO1 (RF),(R1),(1,CHKDATE),(8,DLDSNARR+26)                            
         MVC   DLDSNARR+35(12),=C'DEPOSITED ON'                                 
         GOTO1 (RF),(R1),(1,DEPDATE),(8,DLDSNARR+48)                            
*&&                                                                             
         ZIC   R1,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,R1                                                            
         USING DLPOSTD,R5                                                       
         XC    DLPSEL(113),DLPSEL                                               
         MVI   DLPSEL,X'69'        BUILD X'69' ELEMENT                          
         MVI   DLPSLEN,113                                                      
         MVC   DLPSDBAC,DRAC                                                    
         MVC   DLPSDBNM,DRNM                                                    
         MVC   DLPSCRAC,CRAC                                                    
         MVC   DLPSCRNM,CRNM                                                    
         ZAP   DLPSAMNT,0(6,R3)                                                 
         MVC   DLPSANAL,SPACES                                                  
         MVC   DLPSANAL,6(R6)                                                   
         OC    DLPSANAL,SPACES                                                  
*                                                                               
         LA    RF,DLPOSTD          SAVE ADDRESS OF CURRENT ELEMENT              
         BAS   RE,BLDRAL           CASH ALLOCATION ELEMENT                      
*                                                                               
         ZIC   R1,DLPSLEN          BUMP TO NEXT ELEMENT                         
         AR    R5,R1                                                            
         MVC   0(113,R5),0(RF)     DUPLICATE DATA                               
         MVI   DLPSEL,X'6A'        BUILD X'6A' ELEMENT                          
         MVC   DLPSDBAC,SPACES                                                  
         MVC   DLPSDBAC+3(12),LASTLEFT                                          
         MVC   DLPSDBAC(3),FIRST3                                               
         OC    DLPSDBAC(3),DLPSDBAC FIRST 3 CANNOT BE LESS                      
         BNZ   *+10                                                             
         MVC   DLPSDBAC(3),SPACES   THAN SPACES                                 
         MVC   DLPSDBNM,SPACES                                                  
         ZAP   DLPSAMNT,0(6,R3)                                                 
         SP    DLPSAMNT,0(6,R6)    TAKE OFF CASH DISCOUNT                       
         OC    INCOMAC,INCOMAC                                                  
         BZ    BUILDT4                                                          
         CP    0(6,R6),=P'0'                                                    
         BE    BUILDT4                                                          
         ZIC   R1,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R1,R5                                                            
         MVC   0(113,R1),0(R5)     DUPLICATE DATA                               
         LR    R5,R1                                                            
         MVC   DLPSDBAC,CRAC       BUILD CREDIT INCOME POSTING                  
         MVC   DLPSDBNM,CRNM                                                    
         MVC   DLPSCRAC,INCOMAC                                                 
         MVC   DLPSCRNM,INCOMNM                                                 
         ZAP   DLPSAMNT,0(6,R6)                                                 
BUILDT4  ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         MVI   0(R5),0             SET END OF RECORD                            
         LA    R1,IOAREA-1                                                      
         SR    R5,R1                                                            
         STH   R5,DUB                                                           
         MVC   IOAREA(2),DUB       SET L'RECORD                                 
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
         USING RALELD,R5           R5=A(ALLOCATION ELEMENT)                     
BLDRAL   SR    R1,R1               BUILD RECEIVABLE ALLOCATION ELEMENT          
         IC    R1,RALLN            BUMP TO NEXT OUTPUT ELEMENT                  
         AR    R5,R1                                                            
         XC    RALELD(RALALCLQ),RALELD                                          
         MVI   RALEL,RALELQ                                                     
         MVI   RALLN,RALALCLQ                                                   
         MVI   RALTYPE,RALTALC                                                  
         MVC   RALAREF,CHKNUM                                                   
         MVC   RALADAT,CHKDATE                                                  
         MVC   RALADEP,DEPDATE                                                  
         BR    RE                                                               
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE TO LOAD A NEW SCREEN                                     
NEWSCRN  NTR1                                                                   
         IC    R1,0(R1)                                                         
         MVC   DMCB+4(4),=X'D9060200'                                           
         STC   R1,DMCB+7                                                        
         STC   R1,SCREEN                                                        
         GOTO1 CALLOV,DMCB,(0,CALOLAYH)                                         
         LA    R3,CONHEADH         RE-TRANSMIT TOP OF SCREEN                    
         SR    R4,R4                                                            
         LA    R5,CALOLAYH-1                                                    
         OI    6(R3),X'80'                                                      
         IC    R4,0(R3)                                                         
         BXLE  R3,R4,*-8                                                        
         XIT1                                                                   
         SPACE 1                                                                
TOTLNE   NTR1                                                                   
         LA    R5,CALTOT1H         PUT OUT LAST SCREEN AND BATCH TOTALS         
         CLI   SCREEN,OVSCRN                                                    
         BNE   *+8                                                              
         LA    R5,CALOVT1H                                                      
         XC    8(L'CALTOT1,R5),8(R5)                                            
         OI    6(R5),X'80'                                                      
         MVC   8(25,R5),TOTMESS1                                                
         LA    R3,34(R5)                                                        
         EDIT  LTOTAL,(14,0(R3)),2,ALIGN=LEFT,MINUS=YES                         
         SPACE 1                                                                
         LA    R5,CALTOT2H                                                      
         CLI   SCREEN,OVSCRN                                                    
         BNE   *+8                                                              
         LA    R5,CALOVT2H                                                      
         XC    8(L'CALTOT2,R5),8(R5)                                            
         OI    6(R5),X'80'                                                      
         MVC   8(12,R5),TOTMESS2                                                
         LA    R3,21(R5)                                                        
         EDIT  LTOTALT,(14,0(R3)),2,ALIGN=LEFT,MINUS=YES                        
         XIT1                                                                   
         EJECT                                                                  
*              HANDLE EXITS                                                     
*                                                                               
ALLEXIT  XC    CONHEAD,CONHEAD                                                  
         MVC   MSG,SPACES                                                       
         OI    CONHEADH+6,X'80'                                                 
ALLEX2   CLI   MODE,X'FF'                                                       
         BNE   ALLEX4                                                           
         CLI   SCREEN,OVSCRN                                                    
         BE    ALLEX2A             FINISHED WITH EXTRA SCREEN                   
         TM    OPT1,OVER                                                        
         BZ    ALLEX2B             DON'T WANT EXTRA SCREEN                      
         GOTO1 NEWSCRN,DMCB,(X'D4',0)                                           
         BAS   RE,TOTLNE                                                        
         MVI   MODE,1              SET NEW SCREEN                               
         B     DISPOVER                                                         
ALLEX2A  GOTO1 NEWSCRN,DMCB,(X'DF',0)                                           
         BAS   RE,TOTLNE                                                        
ALLEX2B  L     R1,ACRAC                                                         
         XC    0(15,R1),0(R1)      CLEAR LAST USED ENTRY IN CRACTAB             
         LA    R1,15(R1)                                                        
         ST    R1,ACRAC            SET A(NEXT ENTRY)                            
         CLI   0(R1),X'FF'         LAST ENTRY                                   
         BE    ALLEX3              YES - EXIT                                   
         XC    LKEY,LKEY                                                        
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),0(R1)                                                    
         BAS   RE,GETACC                                                        
         MVC   CRAC,ACCTNUM                                                     
         MVC   CRNM,ACCTNAME                                                    
         MVI   MODE,0                                                           
         CLI   COUNT,0                                                          
         BE    DISPLAY                                                          
         MVI   MODE,1                                                           
         B     DISPLAY                                                          
ALLEX3   DS    0H                                                               
         XC    SAVE1,SAVE1                                                      
         XC    SAVE2,SAVE2                                                      
         MVC   MSG(28),=C'NO MORE DATA TO BE DISPLAYED'                         
         LA    R2,CONACTH                                                       
         B     ALLX                                                             
ALLEX4   CLI   COUNT,0                                                          
         BNE   *+12                                                             
         MVI   MODE,X'FF'                                                       
         B     ALLEX2                                                           
         LA    R2,CALAMNTH                                                      
         CLI   SCREEN,OVSCRN                                                    
         BNE   *+8                                                              
         LA    R2,CALBILLH                                                      
         MVC   MSG(27),=C'INPUT ACCEPTED - ENTER NEXT'                          
         CLI   MODE,1                                                           
         BE    *+10                                                             
         MVC   MSG(30),=C'DATA DISPLAYED - ENTER AMOUNTS'                       
         MVI   MODE,1                                                           
ALLX     MVI   ERRNUM,X'FE'                                                     
         B     EXIT                                                             
         EJECT                                                                  
       ++INCLUDE ACBATCODE                                                      
*              LITERALS ETC.                                                    
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TABLE OF VALID OPTIONS FILTERS                                   
*                                                                               
         DS    0F                                                               
OPTTAB   DS    0CL12                                                            
         DC    CL8'BILL#   ',A(VALBILL)                                         
         DC    CL8'SDATE   ',A(VALDATE)                                         
         DC    CL8'EDATE   ',A(VALDATE)                                         
         DC    CL8'SWITCH  ',A(VALSWIT)                                         
*&&US*&& DC    CL8'CD      ',A(VALINC)                                          
*&&US*&& DC    CL8'GROSS   ',A(VALINC)                                          
*&&UK*&& DC    CL8'SCH     ',A(VALINC)                                          
         DC    CL8'SBILL#  ',A(VALSBIL)                                         
         DC    CL8'DDATE   ',A(VALDDATE)                                        
         DC    CL8'OVER    ',A(VALOVER)                                         
         DC    X'00'                                                            
*                                                                               
MONTHS   DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
*                                                                               
TOTMESS1 DC    C'TOTAL FOR PREVIOUS SCREEN'                                     
TOTMESS2 DC    C'TOTAL SO FAR'                                                  
         SPACE 1                                                                
OVRLIST  DC    CL12'UNAPPLIED'                                                  
         DC    CL12'OVERPAYMENT'                                                
         DC    CL12'OTHERS'                                                     
         DC    X'FF'                                                            
         EJECT                                                                  
* ACBATDSECT                                                                    
       ++INCLUDE ACBATDSECT                                                     
* ACBATE0D                                                                      
       ++INCLUDE ACBATE0D                                                       
         ORG   CALOLAYH                                                         
* ACBATDFD                                                                      
       ++INCLUDE ACBATDFD                                                       
         ORG   CALOLAYH                                                         
* ACBATD4D                                                                      
       ++INCLUDE ACBATD4D                                                       
         ORG   TWAHOLE                                                          
SAVE1    DS    0CL58                                                            
LKEY     DS    CL49                                                             
CHKNUM   DS    CL6                                                              
CHKDATE  DS    CL3                                                              
DEPDATE  DS    CL3                                                              
SAVE2    DS    0CL39                                                            
CRAC     DS    CL15                                                             
DRAC     DS    CL15                                                             
LTOTAL   DS    PL8                                                              
LTOTALT  DS    PL8                                                              
COUNT    DS    CL1                                                              
DISCTAB  DS    CL200                                                            
CRACTAB  DS    10CL15                                                           
FIRST3   DS    CL3                                                              
SRCETBL  DS    (3*MAXCOUNT)C          TABLE OF FIRST 3 OF CONTRA                
*                                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
         EJECT                                                                  
*              DSECT TO COVER TEMP W/S                                          
*                                                                               
PROGD    DSECT                                                                  
MYRELO   DS    F                                                                
SAVERD   DS    F                                                                
ACRAC    DS    A                                                                
*                                                                               
NLINES   DS    CL1                                                              
LINES    DS    7CL32                                                            
*                                                                               
CASHTAB  DS    0CL150                                                           
         DS    25CL6                                                            
*                                                                               
FILTERS  DS    0CL49                                                            
SRCEFILT DS    CL13                                                             
BILLFILT DS    CL7                                                              
SBILFILT DS    CL6                                                              
SDATFILT DS    CL3                                                              
EDATFILT DS    CL3                                                              
SWITCH   DS    CL1                                                              
OPT1     DS    CL1                                                              
DDATE    EQU   X'80'               DON'T ADD BILLS WITH DIFFERENT DATES         
OVER     EQU   X'40'               SHOW UNPROTECTED FOR OVERPAYMENTS            
INCOMAC  DS    CL15                                                             
INCOMNM  DS    CL36                                                             
*                                                                               
LASTS    DS    0CL21                                                            
LSOURCE  DS    CL12                                                             
LREF     DS    CL6                                                              
LDATE    DS    CL3                                                              
LAMOUNT  DS    PL6                                                              
*                                                                               
LASTLEFT DS    CL12                                                             
LASTRGHT DS    CL12                                                             
THISSIDE DS    CL1                                                              
*                                                                               
CRNM     DS    CL36                                                             
DRNM     DS    CL36                                                             
*                                                                               
         SPACE 1                                                                
WKLINE   DS    5CL54                                                            
KEY      DS    CL49                                                             
IOAREA   DS    2000C                                                            
PROGDX   DS    0C                                                               
*                                                                               
OVSCRN   EQU   X'D4'                                                            
MAXCOUNT EQU   25                                                               
         EJECT                                                                  
*              DSECT TO COVER TWA DISPLAY LINE                                  
*                                                                               
LINED    DSECT                                                                  
         DS    CL8                                                              
LINSRCE  DS    CL12                                                             
LINBILL  DS    CL6                                                              
         DS    CL1                                                              
LINDATE  DS    CL7                                                              
LINAMNTH DS    CL8                                                              
LINSWIT  DS    CL1                                                              
LINAMNT  DS    CL11                                                             
LINNEXT  EQU   *-LINED                                                          
*                                                                               
*              ERROR NUMBER EQUATES                                             
*                                                                               
EMIF     EQU   1                                                                
EIIF     EQU   2                                                                
EFNN     EQU   3                                                                
EIPL     EQU   9                                                                
EIIA     EQU   25                                                               
EIDF     EQU   13                                                               
EDNC     EQU   64                                                               
EIAC     EQU   18                                                               
EAIL     EQU   18                                                               
EIAS     EQU   12                                                               
EDIF     EQU   35                                                               
EFTL     EQU   36                                                               
EFTS     EQU   37                                                               
ELCD     EQU   71                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACBAT1E   05/01/02'                                      
         END                                                                    
