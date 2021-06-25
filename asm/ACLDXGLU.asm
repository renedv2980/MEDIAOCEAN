*          DATA SET ACLDXGLU   AT LEVEL 014 AS OF 04/27/00                      
*PHASE ACLDXGLU,*                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE ACRECTYP                                                               
*INCLUDE DATCON                                                                 
         TITLE 'GENERAL ACLD EXTERNAL'                                          
*        CHECK GENERAL LEDGER FIX BAD REF DATES                                 
*--------------------------------------------------------------------*          
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= RECORD IS I/S FILE RECORD            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*--------------------------------------------------------------------*          
         PRINT NOGEN                                                            
DMLDELS  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDELS                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
*--------------------------------------------------------------------*          
* CONTROL FLOW LOGIC                                                            
*--------------------------------------------------------------------*          
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
                                                                                
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
                                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
                                                                                
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
                                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
                                                                                
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
                                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
                                                                                
DMXIT    XMOD1 1                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*----------------------------------------------------------------*              
DMXINIT  DS    0H                                                               
*        BAS   RE,PRNTHDR                                                       
         MVI   SAVECPY,0                                                        
         B     DMXIT                                                            
         EJECT                                                                  
*----------------------------------------------------------------*              
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*----------------------------------------------------------------*              
         USING TRNRECD,R4                                                       
DMXREC   L     R4,AREC                                                          
         GOTO1 =V(ACRECTYP),DMCB,(C'D',TRNRECD)                                 
         CLI   0(R1),ACRTTRN       ONLY WANT TRANS                              
         BNE   DMXKEEP                                                          
         CLI   SAVECPY,0                                                        
         BE    DMXREC01                                                         
         CLC   SAVECPY,TRNKCPY                                                  
         BE    DMXREC02                                                         
         LA    R6,CPYRECS                                                       
         BAS   RE,PRTTOT                                                        
*                                                                               
DMXREC01 ZAP   MAXDUMP,=P'10'                                                   
         ZAP   CPY#GB,=P'0'                                                     
         ZAP   CPY#GP,=P'0'                                                     
         ZAP   CPY#S9,=P'0'                                                     
         MVC   SAVECPY,TRNKCPY                                                  
*                                                                               
DMXREC02 CLC   TRNKULA(2),=C'GB'                                                
         BE    DMXREC04                                                         
         CLC   TRNKULA(2),=C'GP'                                                
         BE    DMXREC04                                                         
         CLC   TRNKULA(2),=C'S9'                                                
         BNE   DMXKEEP                                                          
*                                                                               
DMXREC04 CLI   TRNRSTYP,X'19'      TYPE 25 ONLY                                 
         BNE   DMXKEEP                                                          
         CLI   TRNKREF,X'FA'       IS IT NONE PRINTABLE ?                       
         BL    DMXKEEP                                                          
         BE    *+6                                                              
         DC    H'00'               JUST CHECKING                                
*                                                                               
         AP    PKCOUNT,=P'1'                                                    
         CLI   TRNKULA+1,C'B'      GB ?                                         
         BNE   *+16                                                             
         AP    TOT#GB,=P'1'                                                     
         AP    CPY#GB,=P'1'                                                     
*                                                                               
         CLI   TRNKULA+1,C'P'      GP ?                                         
         BNE   *+16                                                             
         AP    TOT#GP,=P'1'                                                     
         AP    CPY#GP,=P'1'                                                     
*                                                                               
         CLI   TRNKULA+1,C'9'      S9 ?                                         
         BNE   *+16                                                             
         AP    TOT#S9,=P'1'                                                     
         AP    CPY#S9,=P'1'                                                     
*                                                                               
         CP    PKCOUNT,MAXDUMP                                                  
         BH    DMXREC10                                                         
         SR    R5,R5                                                            
         ICM   R5,3,TRNRLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,=C'BEFORE',(R4),C'DUMP',(R5),=C'1D',    X        
               =V(PRINT)                                                        
*                                                                               
DMXREC10 MVI   TRNKREF,C'0'        YEAR 2000                                    
*        CLI   TRNKCPY,X'76'       WANT ONLY RAPP COLLINS                       
*        BNE   DMXKEEP                                                          
*        CLC   TRNKULA,=CL14'SJ905RCWX86293'                                    
*        BNE   DMXKEEP                                                          
*        TM    TRNRSTA2,TRNSGLUP                                                
*        BO    DMXKEEP                                                          
*                                                                               
         USING TRNELD,R2                                                        
         LA    R2,TRNRFST                                                       
         CLI   0(R2),TRNELQ        X'44'                                        
         BE    *+6                                                              
         DC    H'00'               JUST CHECKING                                
*                                                                               
         CLI   TRNREF,X'FA'                                                     
         BE    *+6                                                              
         DC    H'00'               JUST CHECKING                                
*                                                                               
         MVI   TRNREF,C'0'                                                      
         CP    PKCOUNT,MAXDUMP                                                  
         BH    DMXKEEP                                                          
         SR    R5,R5                                                            
         ICM   R5,3,TRNRLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,=C'AFTER',(R4),C'DUMP',(R5),=C'1D'               
         B     DMXKEEP                                                          
*&&DO                                                                           
         LR    R5,R2                     POINT TO TRAN ELEMENT                  
LOOP     CLI   0(R5),0                                                          
         BE    DMXKEEP                                                          
*                                                                               
         CLI   0(R5),TRSELQ              IS IT X'60'                            
         BE    DMXREC5                   NO EXIT                                
         ZIC   R1,1(R5)                  GET LENGTH                             
         AR    R5,R1                     BUMP TO X'60' ELEMENT                  
         B     LOOP                                                             
*                                                                               
         USING TRSELD,R5                                                        
DMXREC5  CLC   TRSPMOS,ADATE             IS DATE SEP 98                         
*        BNE   DMXKEEP       ***TAKE STAR OUT TO CHK FOR DATE*****              
*                                                                               
         USING PLINED,R7                                                        
         LA    R7,P                                                             
*                                                                               
         GOTO1 =V(DATCON),DMCB,(2,TRSDATE),(8,PRTADATE)                         
*                                                                               
         MVC   WORK(2),TRSPMOS                                                  
         MVI   WORK+2,X'01'                                                     
         GOTO1 =V(DATCON),DMCB,(1,WORK),(0,WORK+6)                              
         MVC   PRTMOS,WORK+6                                                    
*                                                                               
         LA    R1,PKDB                   ASSUME NOT DRAFT                       
         TM    TRNRSTA,TRNSDRFT          SKIP DRAFT TRNS                        
         BNO   *+14                                                             
         LA    R1,PKDBDFT                DRAFT                                  
         MVC   PRTDFT,=CL3'***'          FLAG THAT IT'S DRAFT TRAN              
*                                                                               
         TM    TRNSTAT,TRNSDR            IS IT DEBIT/CREDIT                     
         BZ    DMXREC10                                                         
         AP    0(L'PKDB,R1),TRNAMNT      ADD TO DEBITS TOTAL.                   
         EDIT  (P6,TRNAMNT),PRTDAMNT,2,ZERO=NOBLANK,MINUS=YES                   
         B     DMXREC20                                                         
*                                                                               
DMXREC10 DS    0H                                                               
         AP    8(L'PKCR,R1),TRNAMNT          CREDIT TOTAL                       
         EDIT  (P6,TRNAMNT),PRTCAMNT,2,ZERO=NOBLANK,MINUS=YES                   
*                                                                               
DMXREC20 MVC   PRTUNIT(L'PRTUNIT+L'PRTLDG),TRNKUNT   UNIT CODE                  
         MVC   PRTACT,TRNKACT            ACCOUNT CODE                           
*                                                                               
         EDIT  (1,TRNTYPE),PRTTYPE                                              
*                                                                               
         MVC   PRTULC,TRNKULC            CONTRA UNIT,LEDGER,ACCOUNT             
         GOTO1 =V(DATCON),DMCB,(1,TRNKDATE),(8,PRTDATE)                         
         MVC   PRTREF,TRNKREF            REFERENCE NUMBER                       
*                                                                               
         AP    PKCOUNT,=P'1'             INC RECD TOTAL                         
*                                                                               
         BAS   RE,PBOX                   PRNT C'|' AFTR EVRY DATA FLD           
         GOTO1 VPRINTER                  PRINT LINE OF REPORT                   
*                                                                               
         BAS   RE,NEWPGHDR               PRINT HEADER IF IT'S NEW PAGE          
*                                                                               
DMXRECX  B     DMXKEEP                                                          
         DROP  R2,R4,R5,R7                                                      
*&&                                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
*----------------------------------------------------------------*              
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
*----------------------------------------------------------------*              
DMXRET   DS    0H                                                               
                                                                                
*----------------------------------------------------------------*              
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*----------------------------------------------------------------*              
DMXEOF   DS    0H                                                               
*                                                                               
*        BAS   RE,PRNTLINE               CLOSE LAST BOX                         
*                                                                               
*        AP    PKDGRND,PKDB              ADD TO GRAND TOT,DEBIT TOT             
*        AP    PKDGRND,PKDBDFT           ADD TO GRAND TOT,DB/DRAFT TOT          
*                                                                               
*        AP    PKCGRND,PKCR              ADD TO GRAND TOT CREDIT TOT            
*        AP    PKCGRND,PKCRDFT           ADD TO GRAND TOT CR/DRAFT TOT          
*                                                                               
         CP    PKCOUNT,=P'0'                                                    
         BE    DMXEOFX                                                          
         GOTO1 VPRINTER                  PRINT BLANK LINE                       
         LA    R6,TOTRECS                                                       
         BAS   RE,PRTTOT                                                        
*                                                                               
DMXEOFX  B     EXIT                                                             
         EJECT                                                                  
         USING TOTRECD,R6                                                       
         USING PLINED,R7                                                        
PRTTOT   NTR1                                                                   
         LA    R7,P                                                             
         ZAP   TOT#REC,TOTGB                                                    
         AP    TOT#REC,TOTGP                                                    
         AP    TOT#REC,TOTS9                                                    
         CP    TOT#REC,=P'0'                                                    
         BE    PRTTOTX                                                          
         MVC   PRTCMSG,=CL15'TOTAL RECORDS= '                                   
         EDIT  (P8,TOT#REC),PRTCOUNT,0,MINUS=YES,ZERO=NOBLANK                   
         GOTO1 VPRINTER                                                         
         MVC   PRTCMSG,=CL15'TOTAL GB     = '                                   
         EDIT  (P8,TOTGB),PRTCOUNT,0,MINUS=YES,ZERO=NOBLANK                     
         GOTO1 VPRINTER                                                         
         MVC   PRTCMSG,=CL15'TOTAL GP     = '                                   
         EDIT  (P8,TOTGP),PRTCOUNT,0,MINUS=YES,ZERO=NOBLANK                     
         GOTO1 VPRINTER                                                         
         MVC   PRTCMSG,=CL15'TOTAL S9     = '                                   
         EDIT  (P8,TOTS9),PRTCOUNT,0,MINUS=YES,ZERO=NOBLANK                     
         GOTO1 VPRINTER                                                         
PRTTOTX  XIT1                                                                   
         DROP  R6,R7                                                            
         EJECT                                                                  
********************************************************************            
* FORCE NEW PAGE IF PAGE HAS 25 LINES AND PRINT HEADER AT THE      *            
* BEGINING OF EVERY PAGE                                           *            
********************************************************************            
NEWPGHDR NTR1                                                                   
         AP    PKLNCNT,=P'1'             INC NUM OF LINES COUNTER               
         CP    PKLNCNT,MAXLN                                                    
         BNH   NEWPGX                    ONLY PRINT 26 LINES A PAGE             
*                                                                               
         BAS   RE,PRNTLINE               CLOSE BOX                              
*                                                                               
         AP    LINE,MAXLINE              FORCE NEW PAGE                         
         BAS   RE,PRNTHDR                                                       
NEWPGX   XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* PRINT HEADER OF THE REPORT                                         *          
**********************************************************************          
PRNTHDR  NTR1                                                                   
         USING PHEADERD,R7                                                      
         LA    R7,P                                                             
         ZAP   PKLNCNT,=P'0'             RESET NUM OF LINES COUNTER             
*                                                                               
         BAS   RE,PRNTLINE                                                      
*                                                                               
         MVC   PUNITLDG,=CL3'U/L'                                               
         MVC   PACT,=CL12' ACCOUNT'                                             
         MVC   PTYPE(3),=C'TYP'                                                 
         MVC   PULC,=CL14'  CONTRA/ACC'                                         
         MVC   PDATE,=CL8' DATE'                                                
         MVC   PADATE,=CL8'ACT/DATE'                                            
         MVC   PMOS,=CL6' MOS'                                                  
         MVC   PREF,=CL6'REF'                                                   
         MVC   PDAMNT+8(5),=C'DEBIT'                                            
         MVC   PCAMNT+8(6),=C'CREDIT'                                           
         MVC   PDFT,=C'DFT'                                                     
         BAS   RE,PBOX                                                          
         GOTO1 VPRINTER                                                         
*                                                                               
         BAS   RE,PRNTLINE                                                      
*                                                                               
         XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT HORIZONTAL LINE                                              *          
**********************************************************************          
PRNTLINE NTR1                                                                   
         MVI   P,C'-'                    DRAW LINE                              
         MVC   P+1(LINELNQ),P                                                   
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* PRINT BOX VERTICAL LINES                                           *          
**********************************************************************          
PBOX     NTR1                                                                   
         USING PLINED,R7                                                        
         MVI   B1,BAR                                                           
         MVI   B2,BAR                                                           
         MVI   B3,BAR                                                           
         MVI   B4,BAR                                                           
         MVI   B5,BAR                                                           
         MVI   B6,BAR                                                           
         MVI   B7,BAR                                                           
         MVI   B8,BAR                                                           
         MVI   B9,BAR                                                           
         MVI   B10,BAR                                                          
         MVI   B11,BAR                                                          
         MVI   B12,BAR                                                          
         XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE AND CONSTANTS                                       *         
**********************************************************************          
         SPACE 1                                                                
PKCOUNT  DC    PL8'0'                                                           
*                                                                               
* KEEP THESE FOUR ALWAYS TOGETHER                                               
*                                                                               
PKDB     DC    PL8'0'                    DEBIT TOTALS                           
PKCR     DC    PL8'0'                    CREDIT TOTALS                          
PKDBDFT  DC    PL8'0'                    DEBIT DRAFT TOTALS                     
PKCRDFT  DC    PL8'0'                    CREDIT DRAFT TOTALS                    
PKDGRND  DC    PL8'0'                    GRAND TOTALS FOR DEBIT                 
PKCGRND  DC    PL8'0'                    GRAND TOTAL FOR CREDIT                 
*                                                                               
TOTRECS  DS    0PL8                                                             
TOT#GB   DC    PL8'0'                    GRAND TOTAL FOR GB                     
TOT#GP   DC    PL8'0'                    GRAND TOTAL FOR GP                     
TOT#S9   DC    PL8'0'                    GRAND TOTAL FOR S9                     
*                                                                               
CPYRECS  DS    0PL8                                                             
CPY#GB   DC    PL8'0'                    COMPANY TOTAL FOR GB                   
CPY#GP   DC    PL8'0'                    COMPANY TOTAL FOR GP                   
CPY#S9   DC    PL8'0'                    GRAND TOTAL FOR S9                     
*                                                                               
TOT#REC  DC    PL8'0'                                                           
*                                                                               
ADATE    DC    XL2'9809'                 SEP/98                                 
BAR      EQU   C'|'                                                             
MAXLN    DC    PL4'35'                   MAXIMUM LINES PER PAGE                 
PKLNCNT  DS    PL4'0'                                                           
MAXDUMP  DC    P'10'                                                            
SAVECPY  DS    XL1                                                              
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
TOTRECD  DSECT                                                                  
TOTGB    DS    PL8                                                              
TOTGP    DS    PL8                                                              
TOTS9    DS    PL8                                                              
         EJECT                                                                  
**********************************************************************          
* PRINT HEADER DSECT                                                 *          
**********************************************************************          
         SPACE 1                                                                
PHEADERD DSECT                                                                  
         DS    CL1                                                              
PUNITLDG DS    CL3                       UNIT                                   
         DS    CL1                                                              
PACT     DS    CL12                      LEVLA/LEVLB/LEVLC/LEVLD                
         DS    CL2                                                              
PTYPE    DS    CL2                       TRANSACTION TYPE 48                    
         DS    CL2                                                              
PULC     DS    CL14                      CONTRA UNIT,LEDGER,ACCOUNT             
         DS    CL2                                                              
PDATE    DS    CL8                       DATE                                   
         DS    CL2                                                              
PADATE   DS    CL8                       ACTIVITY DATE                          
         DS    CL2                                                              
PMOS     DS    CL6                                                              
         DS    CL2                                                              
PREF     DS    CL6                       REFERENCE NUM.                         
         DS    CL2                                                              
PDAMNT   DS    CL16                      PRINT DEBIT AMOUNT                     
         DS    CL2                                                              
PCAMNT   DS    CL16                      PRINT CREDIT AMOUNT                    
         DS    CL1                                                              
PDFT     DS    CL3                       DRAFT TRAN  ***                        
LINELNQ  EQU   *-PHEADERD                UNDERLINE HEADERS  EQU                 
         EJECT                                                                  
**********************************************************************          
* PRINT LINE DSECT                                                   *          
**********************************************************************          
         SPACE 1                                                                
PLINED   DSECT                                                                  
B1       DS    CL1                       C'|' FOR A BOX                         
PRTUNIT  DS    CL1                       UNIT                                   
PRTLDG   DS    CL1                       LEDGER                                 
         DS    CL1                                                              
B2       DS    CL2                       C'|' FOR BOX                           
PRTACT   DS    CL12                      LEVLA/LEVLB/LEVLC/LEVLD                
B3       DS    CL2                                                              
PRTTYPE  DS    CL2                       TRANSACTION TYPE 48                    
B4       DS    CL2                                                              
PRTULC   DS    CL14                      CONTRA UNIT,LEDGER,ACCOUNT             
B5       DS    CL2                                                              
PRTDATE  DS    CL8                       DATE                                   
B6       DS    CL2                                                              
PRTADATE DS    CL8                       ACTIVITY DATE                          
B7       DS    CL2                                                              
PRTMOS   DS    CL4                                                              
B8       DS    CL2                                                              
PRTREF   DS    CL6                       REFERENCE NUM.                         
B9       DS    CL2                                                              
PRTDAMNT DS    CL16                      PRINT DEBIT AMOUNT                     
B10      DS    CL2                                                              
PRTCAMNT DS    CL16                      PRINT CREDIT AMOUNT                    
B11      DS    CL2                                                              
PRTDFT   DS    CL3                       PRINT CREDIT AMOUNT                    
B12      DS    CL2                                                              
*                                                                               
         ORG   PLINED                                                           
         DS    CL20                                                             
PRTCMSG  DS    CL15'NUM OF RECDS = '                                            
PRTCOUNT DS    CL6                       NUM OF RECDS                           
         DS    CL15                                                             
PRTAMSG  DS    CL15'TOTAL        = '                                            
         ORG                                                                    
*                                                                               
         ORG   PRTAMSG                                                          
PRTDMSG  DS    CL15'DRAFT TOTAL ='       DRAFT TRAN TOTALS MSG                  
         ORG                                                                    
*                                                                               
         ORG   PRTAMSG                                                          
PRTTMSG  DS    CL15'TOTAL AMOUNT ='      GRAND TOTALS MSG                       
         ORG                                                                    
*                                                                               
         ORG   PRTDAMNT                                                         
PRTDTOTL DS    CL16                      PRINT TOTAL AMOUNT                     
         ORG                                                                    
         ORG   PRTCAMNT                                                         
PRTCTOTL DS    CL16                      PRINT TOTAL AMOUNT                     
         ORG                                                                    
         EJECT                                                                  
**********************************************************************          
* OTHER DSECTS                                                       *          
**********************************************************************          
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
HALF     DS    H                                                                
WORK     DS    CL64                                                             
                                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
ELEMENT  DS    XL255                                                            
RECTYP   DS    CL1                                                              
WORKX    EQU   *                                                                
         EJECT                                                                  
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014ACLDXGLU  04/27/00'                                      
         END                                                                    
