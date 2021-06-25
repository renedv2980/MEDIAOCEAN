*          DATA SET PREXTJWB   AT LEVEL 050 AS OF 05/01/02                      
*PHASE PREXTJWB,+0                                                              
*INCLUDE BINSRCH                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE GETINS                                                                 
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
DMLDEXT  CSECT                                                                  
         NMOD1 80,DMLDEXT                                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
*                                                                               
         EJECT                                                                  
* PROCESS RECORD LOGIC                                                          
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         USING PBUYRECD,R3                                                      
*                                                                               
         CLC   0(2,R3),=C'JW'      ONLY CHECK AGENCY                            
         BNE   DMXKEEP                                                          
         CLI   3(R3),X'20'         SEE IF BUYREC                                
         BNE   DMXKEEP                                                          
*                                                                               
         MVI   ERR,C' '                                                         
         MVI   CHGSW,0                                                          
*                                                                               
*                                    STILL DO OTHER FIX                         
DMXR0    DS    0H                                                               
         LA    R6,PBDELEM                                                       
         MVI   ELCODE,X'66'                                                     
DMXR1    BAS   RE,NEXTEL                                                        
         BNE   DMXKEEP                                                          
         CLC   2(3,R6),=C'==X'      MARK PAID AND BILLED                        
         BE    PAYBILL                                                          
         CLC   2(3,R6),=C'==P'      MARK PAID (USE BILLINGS)                    
         BE    PAY                                                              
         CLC   2(3,R6),=C'==B'      MARK BILLED (USE PAYMENTS)                  
         BE    BILL                                                             
         B     DMXR1                                                            
*                                                                               
*                                                                               
PAY      DS    0H                                                               
         XC    BPGRS(12),BPGRS                                                  
         XC    BBGRS(12),BBGRS                                                  
         LA    R6,PBDELEM                                                       
         MVI   ELCODE,X'26'                                                     
PAY1     BAS   RE,NEXTEL           SEE IF BILLED                                
         BNE   PAY8                                                             
         OC    5(3,R6),5(R6)        CHK FOR DATE                                
         BZ    PAY1                                                             
         LM    R7,R9,BPGRS                                                      
         A     R7,11(R6)                                                        
         A     R8,15(R6)                                                        
         A     R9,19(R6)                                                        
         STM   R7,R9,BPGRS                                                      
         B     PAY1                                                             
*                                                                               
PAY8     OC    BPGRS(12),BPGRS                                                  
         BZ    PAYERR                                                           
         XC    ELEM(30),ELEM                                                    
         MVI   ELEM,X'25'                                                       
         MVI   ELEM+1,X'16'                                                     
         MVC   ELEM+2(3),=X'58091E'        SET TO SEP30/88                      
         MVC   ELEM+5(3),=X'58091E'        SET TO SEP30/88                      
         STM   R7,R9,ELEM+8                                                     
         LA    R6,PBDELEM                                                       
         MVI   ELCODE,X'25'         SEARCH FOR EMPTY PAY ELEM                   
         BAS   RE,NEXTEL                                                        
         BNE   PAYERR3                                                          
         OC    2(3,R6),2(R6)        SEE IF ALREADY PAID                         
         BNZ   PAYERR2                                                          
         MVC   0(22,R6),ELEM        CAN JUST MOVE ELEM                          
         B     PAYX                                                             
*                                                                               
PAYX     MVI   CHGSW,1                                                          
         LM    R7,R9,PGRS                                                       
         A     R7,BPGRS                                                         
         A     R8,BPAGYC                                                        
         A     R9,BPCD                                                          
         STM   R7,R9,PGRS           ROLL TO PAID TOTALS                         
         AP    PCHGCNT,=P'1'                                                    
         B     DMXREC3                                                          
*                                                                               
PAYERR   MVI   ERR,C'1'            NO BILLINGS                                  
         B     DMXREC4                                                          
*                                                                               
PAYERR2  CLC   2(3,R6),=X'58091E'  SEE IF DDS PAYMENT                           
         BE    DMXKEEP             YES - THEN NO ERROR                          
         MVI   ERR,C'2'            ALREADY PAID                                 
         B     DMXREC4                                                          
*                                                                               
PAYERR3  MVI   ERR,C'3'            NO EMPTY PAY ELEMS                           
         GOTO1 =V(RECUP),DMCB,(1,PBUYREC),ELEM,(R6)                             
         B     PAYX                                                             
*                                                                               
BILL     DS    0H                                                               
         XC    BPGRS(12),BPGRS                                                  
         XC    BBGRS(12),BBGRS                                                  
         LA    R6,PBDELEM                                                       
         MVI   ELCODE,X'25'                                                     
BIL1     BAS   RE,NEXTEL           SEE IF PAID                                  
         BNE   BIL8                                                             
         OC    2(3,R6),2(R6)        CHK FOR DATE                                
         BZ    BIL1                                                             
         LM    R7,R9,BBGRS                                                      
         A     R7,8(R6)                                                         
         A     R8,12(R6)                                                        
         A     R9,16(R6)                                                        
         STM   R7,R9,BBGRS                                                      
         B     BIL1                                                             
*                                                                               
BIL8     OC    BBGRS(12),BBGRS                                                  
         BZ    BILERR                                                           
         XC    ELEM(30),ELEM                                                    
         MVI   ELEM,X'26'                                                       
         MVI   ELEM+1,X'17'                                                     
         MVC   ELEM+2(3),PBUYKPRD                                               
         MVC   ELEM+5(3),=X'58091E'        SET TO SEP30/88                      
         STM   R7,R9,ELEM+11                                                    
*                                                                               
*                                   ON PASS 0 LOOK FOR ANY BILL ELEM            
*                                   WITH DATE (SKIPPING REVERSALS)              
*                                   IF NO ERRORS SET PASS TO 1                  
*                                   AND LOOK FOR EMPTY BILL ELEM                
         MVI   PASS,0                                                           
BIL8X    LA    R6,PBDELEM                                                       
         MVI   ELCODE,X'26'         SEARCH FOR EMPTY BILL ELEM                  
BIL9     BAS   RE,NEXTEL                                                        
         BNE   BILERR3                                                          
         TM    10(R6),X'C0'         SKIP REVERSED AND REVERSALS                 
         BZ    BIL12                                                            
         B     BIL9                                                             
*                                                                               
BIL12    OC    5(3,R6),5(R6)        SEE IF ALREADY BILLED                       
         BNZ   BILERR2                                                          
         CLI   PASS,0                                                           
         BE    BIL9                 MUST CONTINUE LOOKING                       
*                                                                               
         MVC   0(23,R6),ELEM        CAN JUST MOVE ELEM                          
         B     BILX                                                             
*                                                                               
BILX     MVI   CHGSW,1                                                          
         LM    R7,R9,BGRS                                                       
         A     R7,BBGRS                                                         
         A     R8,BBAGYC                                                        
         A     R9,BBCD                                                          
         STM   R7,R9,BGRS           ROLL TO BILLED TOTALS                       
         AP    BCHGCNT,=P'1'                                                    
         B     DMXREC3                                                          
*                                                                               
BILERR   MVI   ERR,C'5'            NO PAYMENTS                                  
         B     DMXREC4                                                          
*                                                                               
BILERR2  CLC   5(3,R6),=X'58091E'   ==B BILLED ALREADY                          
         BE    DMXKEEP            NO ERROR                                      
         MVI   ERR,C'6'           ALREADY BILLED                                
         B     DMXREC4                                                          
*                                                                               
*                                                                               
BILERR3  CLI   PASS,1                                                           
         BE    BILERR3C                                                         
         MVI   PASS,1                                                           
         B     BIL8X                                                            
*                                                                               
BILERR3C MVI   ERR,C'7'            NO BILLING ELEM                              
         GOTO1 =V(RECUP),DMCB,(1,PBUYREC),ELEM,(R6)                             
         B     BILX                                                             
*                                                                               
PAYBILL  DS    0H                                                               
         MVI   ERR,C'X'                                                         
         GOTO1 =V(GETINS),DMCB,PBUYREC,GROSS,PBUYKPRD                           
         OC    PGROSS(12),PGROSS                                                
         BZ    BPAY8                                                            
         B     BPAYERR1            ALREADY PAID                                 
*                                                                               
BPAY8    OC    GROSS(12),GROSS      NOTHING TO PAY                              
         BZ    BPAYERR2                                                         
         XC    ELEM(30),ELEM                                                    
         MVI   ELEM,X'25'                                                       
         MVI   ELEM+1,X'16'                                                     
         MVC   ELEM+2(3),=X'58091E'        SET TO SEP30/88                      
         MVC   ELEM+5(3),=X'58091E'        SET TO SEP30/88                      
         MVC   ELEM+8(12),GROSS                                                 
         LA    R6,PBDELEM                                                       
         MVI   ELCODE,X'25'         SEARCH FOR EMPTY PAY ELEM                   
         BAS   RE,NEXTEL                                                        
         BNE   BPAYERR3                                                         
         OC    2(3,R6),2(R6)        SEE IF ALREADY PAID                         
         BNZ   BPAYERR1                                                         
         MVC   0(22,R6),ELEM        CAN JUST MOVE ELEM                          
         B     BPAYX                                                            
*                                                                               
BPAYX    MVI   CHGSW,1                                                          
         LM    R7,R9,PGRS                                                       
         A     R7,GROSS                                                         
         A     R8,AGYCOM                                                        
         A     R9,CSHDSC                                                        
         STM   R7,R9,PGRS           ROLL TO PAID TOTALS                         
         AP    PCHGCNT,=P'1'                                                    
         B     BBILL               NO DO BILLING                                
*                                                                               
BPAYERR1 MVI   ERR,C'P'            ALREADY PAID                                 
         B     DMXREC4                                                          
*                                                                               
BPAYERR2 MVI   ERR,C'N'            NOTHING TO PAY                               
         B     DMXREC4                                                          
*                                                                               
BPAYERR3 DS    0H                  NO EMPTY PAY ELEMS                           
         GOTO1 =V(RECUP),DMCB,(1,PBUYREC),ELEM,(R6)                             
         B     BPAYX                                                            
*                                                                               
BBILL    DS    0H                                                               
         OC    BGROSS(12),BGROSS                                                
         BNZ   BBILERR1                                                         
         B     BBIL8                                                            
*                                                                               
BBIL8    OC    GROSS(12),GROSS                                                  
         BZ    BBILERR2                                                         
         XC    ELEM(30),ELEM                                                    
         MVI   ELEM,X'26'                                                       
         MVI   ELEM+1,X'17'                                                     
         MVC   ELEM+2(3),PBUYKPRD                                               
         MVC   ELEM+5(3),=X'58091E'        SET TO SEP30/88                      
         MVC   ELEM+11(12),GROSS                                                
*                                                                               
*                                   ON PASS 0 LOOK FOR ANY BILL ELEM            
*                                   WITH DATE (SKIPPING REVERSALS)              
*                                   IF NO ERRORS SET PASS TO 1                  
*                                   AND LOOK FOR EMPTY BILL ELEM                
         MVI   PASS,0                                                           
BBIL8X   LA    R6,PBDELEM                                                       
         MVI   ELCODE,X'26'         SEARCH FOR EMPTY BILL ELEM                  
BBIL9    BAS   RE,NEXTEL                                                        
         BNE   BBILERR3                                                         
         TM    10(R6),X'C0'         SKIP REVERSED AND REVERSALS                 
         BZ    BBIL12                                                           
         B     BBIL9                                                            
*                                                                               
BBIL12   OC    5(3,R6),5(R6)        SEE IF ALREADY BILLED                       
         BNZ   BBILERR1                                                         
         CLI   PASS,0                                                           
         BE    BBIL9                                                            
         MVC   0(23,R6),ELEM        CAN JUST MOVE ELEM                          
         B     BBILX                                                            
*                                                                               
BBILX    MVI   CHGSW,1                                                          
         LM    R7,R9,BGRS                                                       
         A     R7,GROSS                                                         
         A     R8,AGYCOM                                                        
         A     R9,CSHDSC                                                        
         STM   R7,R9,BGRS           ROLL TO BILLED TOTALS                       
         AP    BCHGCNT,=P'1'                                                    
         B     DMXREC3                                                          
*                                                                               
BBILERR1 MVI   ERR,C'B'            ALREADY BILLED                               
         B     DMXREC4                                                          
*                                                                               
BBILERR2 MVI   ERR,C'N'           NOTHING TO BILL                               
         B     DMXREC4                                                          
*                                                                               
BBILERR3 DS    0H                  NO EMPTY ELEMS                               
         CLI   PASS,1                                                           
         BE    BERR3C                                                           
         MVI   PASS,1                                                           
         B     BBIL8X             CHECK AGAIN FOR EMPTY BILL ELEMS              
*                                                                               
BERR3C   GOTO1 =V(RECUP),DMCB,(1,PBUYREC),ELEM,(R6)                             
         B     BBILX                                                            
*                                                                               
*                                                                               
DMXREC3  DS    0H                                                               
         CLI   CHGSW,1               SEE IF RECORD CHANGED                      
         BNE   DMXKEEP                                                          
         AP    CHGCOUNT,=P'1'                                                   
DMXREC4  BAS   RE,DDSTRACE                                                      
         B     DMXKEEP                                                          
*                                                                               
**CODE FROM 10/22/88  TO  FIX BAD RECORD LENGTHS                                
         SR    R4,R4                                                            
         MVC   HALF,PBUYLEN                                                     
         LH    R4,HALF                                                          
         LA    R5,PBUYREC                                                       
         AR    R5,R4               POINT TO END OF RECORD                       
         SR    R7,R7                                                            
         LA    R7,33              FOR KEY,ETC                                   
         LA    R6,PBDELEM                                                       
DMXREC5  CLI   0(R6),0                                                          
         BE    DMXREC8                                                          
         ZIC   R0,1(R6)                                                         
         CLI   0(R6),X'25'       TRY TO SAVE 25 ELEMS                           
         BE    DMXREC7                                                          
         CLI   0(R6),X'26'       TRY TO SAVE 26 ELEMS                           
         BE    DMXREC7                                                          
         CLI   0(R6),X'66'       TRY TO SAVE COMMENT ELEMS                      
         BE    DMXREC7                                                          
         CLI   0(R6),X'99'       TRY TO SAVE CONVERSION ELEM                    
         BE    DMXREC7                                                          
         LR    R8,R6             SEE IF THIS ELEM GOES PAST END                 
         AR    R8,R0                                                            
         CR    R8,R5                                                            
         BH    DMXREC8            YES STOP                                      
DMXREC7  AR    R6,R0                                                            
         AR    R7,R0                                                            
         B     DMXREC5                                                          
*                                                                               
DMXREC8  STH   R7,HALF                                                          
         CLC   PBUYLEN,HALF                                                     
         BE    DMXKEEP            RECORD IS OK                                  
         MVC   PBUYLEN,HALF       STORE CORRECTED RECORD LENGTH                 
         LA    R5,PBUYREC                                                       
         AR    R5,R7                                                            
         MVI   0(R5),0             INSURE BINARY ZERO AT END                    
         AP    CHGCOUNT,=P'1'                                                   
         BAS   RE,DDSTRACE                                                      
         B     DMXKEEP                                                          
*                                  END OF BRUCE PLATT'S CODE                    
********************                                                            
**** MEL'S CODE                                                                 
************                                                                    
*                                                                               
DMXRECM  DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         USING PBUYRECD,R3                                                      
*                                                                               
         CLC   0(4,R3),JWNBUY                                                   
         BNE   DMXKEEP                                                          
*                                                                               
         LA    R6,PBDELEM                                                       
         MVI   ELCODE,X'99'                                                     
         BAS   RE,NEXTEL           FIND A CONVERSION ELEMENT                    
         BNE   DMXKEEP                                                          
*                                                                               
         LA    R6,PBDELEM                                                       
         MVI   ELCODE,X'25'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   DMXREC10                                                         
*                                                                               
         USING PPAYELEM,R6                                                      
         OC    PPDDATE,PPDDATE     TEST PAID                                    
         BNZ   DMXKEEP                                                          
         DROP  R6                                                               
*                                                                               
DMXREC10 LA    R6,PBDELEM                                                       
         MVI   ELCODE,X'26'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   DMXREC20                                                         
*                                                                               
         USING PBILELEM,R6                                                      
         OC    PBLDATE,PBLDATE     TEST BILLED PBLDATE,PBLDATE                  
         BNZ   DMXKEEP                                                          
         DROP  R6                                                               
*                                                                               
DMXREC20 DS    0H                                                               
         ZIC   RE,PBUYKDAT         GET INSERTION YEAR                           
         ZIC   RF,PBUYKDAT+1       GET INSERTION MONTH                          
         STC   RE,PBDBDATE                                                      
         STC   RF,PBDBDATE+1                                                    
*                                                                               
         LA    RF,1(RF)                                                         
         CH    RF,=H'12'                                                        
         BNH   *+12                                                             
         LA    RF,1                                                             
         LA    RE,1(RE)                                                         
         STC   RE,PBDPDATE                                                      
         STC   RF,PBDPDATE+1                                                    
*                                                                               
         AP    CHGCOUNT,=P'1'                                                   
         BAS   RE,DDSTRACE                                                      
         B     DMXKEEP                                                          
*                                                                               
JWNBUY   DC    C'JWN',X'20'                                                     
         EJECT                                                                  
NEXTEL   SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   0(1,R6),ELCODE                                                   
         BER   RE                  EXIT WITH CC EQ                              
         B     NEXTEL                                                           
*                                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  EXIT WITH CC NOT EQ                          
         SPACE 1                                                                
         EJECT                                                                  
*=================================*                                             
* SUBROUTINE PRINTS ELEMENT TRACE *                                             
*=================================*                                             
         SPACE 1                                                                
         USING  PBUYRECD,R3                                                     
DDSTRACE NTR1                                                                   
*                                                                               
         CLI   ERR,0                                                            
         BNE   DDSTR1                                                           
         MVI   ERR,C'X'                                                         
         B     DDSTR1                                                           
** BY-PASSED                                                                    
         ZAP   DUB,CHGCOUNT                                                     
         CVB   RE,DUB                                                           
         SRDL  RE,32                                                            
         D     RE,=F'50'                                                        
         C     RE,=F'1'                                                         
         BNE   EXIT                                                             
*                                                                               
DDSTR1   MVC   P(25),PBUYREC                                                    
         MVC   P+35(11),=C'ERROR CODE='                                         
         MVC   P+48(1),ERR                                                      
         CLI   ERR,C'N'                                                         
         BNE   DDSTR1A                                                          
         MVC   P+50(19),=C'NOTHING TO PAY/BILL'                                 
         B     DDSTR1C                                                          
*                                                                               
DDSTR1A  CLI   ERR,C'P'                                                         
         BNE   DDSTR1A5                                                         
         MVC   P+50(12),=C'ALREADY PAID'                                        
         LA    R7,PGROSS                                                        
         B     DDSTR1C                                                          
*                                                                               
DDSTR1A5 CLI   ERR,C'B'                                                         
         BNE   DDSTR1B                                                          
         MVC   P+50(14),=C'ALREADY BILLED'                                      
         LA    R7,BGROSS                                                        
         B     DDSTR1C                                                          
*        FOR ERROR CODES 5-7   BILLING                                          
*                                                                               
DDSTR1B  CLI   ERR,C'X'          NORMAL ==X                                     
         BNE   DDSTR1B5                                                         
         MVC   P+50(11),=C'PAID/BILLED'                                         
         LA    R7,GROSS                                                         
         B     DDSTR1C                                                          
*                                                                               
DDSTR1B5 OC    BPGRS(12),BPGRS      SEE IF PAID                                 
         BZ    DDSTR1B7                                                         
         MVC   P+55(4),=C'PAID'                                                 
         LA    R7,BPGRS                                                         
         B     DDSTR1C                                                          
*                                                                               
DDSTR1B7 OC    BBGRS(12),BBGRS      SEE IF BILLED                               
         BZ    DDSTR1X                                                          
         MVC   P+53(6),=C'BILLED'                                               
         LA    R7,BBGRS                                                         
*                                                                               
*                                                                               
DDSTR1C  LA    R8,3                                                             
         LA    R9,P+70                                                          
DDSTR1F  EDIT  (B4,0(R7)),(14,0(R9)),2,COMMAS=YES,MINUS=YES                     
         LA    R7,4(R7)                                                         
         LA    R9,20(R9)                                                        
         BCT   R8,DDSTR1F                                                       
DDSTR1X  GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,PBUYREC,HEXBLOCK,27,=C'SEP'                      
         MVC   P(27),HEXBLOCK                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(27),HEXBLOCK+27                                                
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(116),PBDELEM                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,PBDELEM,HEXBLOCK,116,=C'SEP'                     
*                                                                               
         MVC   P(116),HEXBLOCK                                                  
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(116),HEXBLOCK+116                                              
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R4,PBDELEM                                                       
*                                                                               
DDSTR2   ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    EXIT                                                             
         ZIC   R5,1(R4)            GET CURRENT ELEMENT LENGTH                   
         BCTR  R5,0                SET FOR EX                                   
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R4) *EXECUTED*                                            
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R5,1(R5)                                                         
         GOTO1 =V(HEXOUT),DMCB,(R4),HEXBLOCK,(R5),=C'SEP'                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P(0),HEXBLOCK *EXECUTED*                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    RE,HEXBLOCK+1(R5)                                                
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(RE) *EXECUTED*                                            
         GOTO1 VPRINTER                                                         
         B     DDSTR2                                                           
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
****************************************************************                
         EJECT                                                                  
         LOAD  EPLOC=JWCONS        LOAD CLIENT KEEP LIST                        
         LR    RE,R0                                                            
         MVC   MYCONPRM(24),0(RE)                                               
         B     DMXIT                                                            
JWCONS   DC    CL8'JWCONS'                                                      
MYCONPRM DC    6F'0'                                                            
DMXNOP   BC    0,DMXKEEP                                                        
         CLC   0(2,R3),=C'JW'                                                   
         BNH   DMXREC2                                                          
         MVI   DMXNOP+1,X'F0'                                                   
         B     DMXKEEP                                                          
*                                                                               
DMXREC2  DS    0H                                                               
         CLC   0(2,R3),=C'JW'                                                   
         BNE   DMXKEEP                                                          
         CLI   3(R3),X'10'         TEST CONTRACT                                
         BNE   DMXKEEP                                                          
         GOTO1 =V(BINSRCH),MYCONPRM,4(R3)                                       
         CLI   0(R1),1             TEST CLIENT FOUND IN LIST                    
         BE    DMXPURGE            NO - PURGE RECORD                            
         GOTO1 =V(HEXOUT),DMCB,(R3),P,7,=C'TOG'                                 
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         LA    R3,COUNTS                                                        
         LA    R4,NCOUNTS                                                       
*                                                                               
DMXEOF2  OI    3(R3),X'0F'                                                      
         UNPK  P(7),0(4,R3)                                                     
         MVC   P+10(20),4(R3)                                                   
         LA    R3,24(R3)                                                        
         GOTO1 VPRINTER                                                         
         BCT   R4,DMXEOF2                                                       
*                                                                               
         LA    R3,PGRS                                                          
         LA    R5,TITLES                                                        
         LA    R4,6                                                             
*                                                                               
DMXEOF5  EDIT  (B4,0(R3)),(14,P+20),2,COMMAS=YES,MINUS=YES                      
         MVC   P+1(15),0(R5)                                                    
         LA    R3,4(R3)                                                         
         LA    R5,15(R5)                                                        
         GOTO1 VPRINTER                                                         
         BCT   R4,DMXEOF5                                                       
*                                                                               
         B     DMXIT                                                            
*                                                                               
         EJECT                                                                  
COUNTS   DS    0CL24                                                            
PCHGCNT  DC    PL4'0',CL20'MARKED PAID'                                         
BCHGCNT  DC    PL4'0',CL20'MARKED BILLED'                                       
CLTCHG   DC    PL4'0',CL20'CLT CHANGES'                                         
CHGCOUNT DC    PL4'0',CL20'CHANGED RECORDS'                                     
NCOUNTS  EQU   (*-COUNTS)/L'COUNTS                                              
         DS    F                                                                
*                                                                               
TITLES   DC    CL15'PAID GROSS'                                                 
         DC    CL15'PAID AGY COM'                                               
         DC    CL15'PAID CD'                                                    
         DC    CL15'BILLED GROSS'                                               
         DC    CL15'BILLED AGY COM'                                             
         DC    CL15'BILLED CD'                                                  
*                                                                               
PGRS     DC    F'0'          PAID TOTALS                                        
PAGYC    DC    F'0'                                                             
PCD      DC    F'0'                                                             
*                                                                               
BGRS     DC    F'0'         BILLED TOTALS                                       
BAGYC    DC    F'0'                                                             
BCD      DC    F'0'                                                             
         SPACE 2                                                                
         LTORG                                                                  
HEXBLOCK DS    CL500                                                            
         SPACE 2                                                                
WORKD    DSECT                                                                  
WORK     DS    CL30                                                             
ELEM     DS    CL50                                                             
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
BYTE     DS    X                                                                
ELCODE   DS    X                                                                
PASS     DS    CL1                                                              
HALF     DS    H                                                                
CHGSW    DS    CL1                                                              
ERR      DS    CL1         1=NOT BILLED     FOR ==P BUYS                        
*                          2=ALREADY PAID   FOR ==P BUYS                        
*                          3=NO EMPTY PAY ELEM FOR ==P BUYS                     
*                                                                               
*                          5=NOT PAID       FOR ==B BUYS                        
*                          6=ALREADY BILLED FOR ==B BUYS                        
*                          7=NO EMPTY BILL ELEM FOR ==B BUYS                    
*                                                                               
*                          C= CLIENT CHANGED                                    
*                                                                               
BPGRS    DS    F                                                                
BPAGYC   DS    F                                                                
BPCD     DS    F                                                                
*                                                                               
BBGRS    DS    F                                                                
BBAGYC   DS    F                                                                
BBCD     DS    F                                                                
*                                                                               
*                          TOTALED FROM GETINS (BEFORE FIX)                     
GROSS    DS    F                                                                
AGYCOM   DS    F                                                                
CSHDSC   DS    F                                                                
PYABLE   DS    F                                                                
BLABLE   DS    F                                                                
PREMIUM  DS    F                                                                
UNITS    DS    F                                                                
*                                                                               
*PAID DATA                                                                      
PGROSS   DS    F                                                                
PAGYCOM  DS    F                                                                
PCSHDSC  DS    F                                                                
PAID     DS    F                                                                
TAX      DS    F                                                                
*                                                                               
*BILLED DATA                                                                    
BGROSS   DS    F                                                                
BAGYCOM  DS    F                                                                
BCSHDSC  DS    F                                                                
BILLED   DS    F                                                                
BLBLDT   DS    CL3                                                              
*                                                                               
         EJECT                                                                  
PBUYRECD DSECT                                                                  
       ++INCLUDE PBUYREC                                                        
       ++INCLUDE PBDELEM                                                        
       ++INCLUDE PPAYELEM                                                       
       ++INCLUDE PBILELEM                                                       
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050PREXTJWB  05/01/02'                                      
         END                                                                    
