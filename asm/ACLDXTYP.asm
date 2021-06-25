*          DATA SET ACLDXTYP   AT LEVEL 003 AS OF 05/20/02                      
*PHASE ACLDXTYP                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE ACRECTYP                                                               
         TITLE 'FIND TRANSACTIONS BASED ON A VARIETY OF CRITERIA'               
*                                                                               
*  INPUT DATA IF PASSED IN PARAM                                                
*  PARAM=BTULULWOSSSSSSEEEEEEAAAAAABBBBBB                                       
*  *     BT                                     BATCH TYPE                      
*  *       UL                                   ACCOUNT UNIT/LEDGER             
*  *         UL                                 CONTRA UNIT/LEDGER              
*  *           WO                               WORKCODE/OFFICE                 
*  *             RRRRRR                         REFERENCE NUMBER                
*  *                   SSSSSS                   TRANSACTION START DATE          
*  *                         EEEEEE             TRANSACTION END DATE            
*  *                               AAAAAA       TRANSACTION ADD DATE            
*  *                                     BBBBBB BILL NUMBER                     
*                                                                               
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
         SPACE 2                                                                
         PRINT NOGEN                                                            
ACLDXTYP CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDELS                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
         ZAP   COMRECS,=P'0'                                                    
         ZAP   TOTRECS,=P'0'                                                    
         ZAP   TOTAMNT,=P'0'                                                    
         ZAP   ITMAMNT,=P'0'                                                    
         XC    SELFLDS,SELFLDS     CLEAR SELECTION CRITERIA                     
         MVC   SELEND,=3X'FF'      END DEFAULT END DATE                         
*                                                                               
         USING PARAMD,R2                                                        
         L     R2,APARAMC                                                       
         CLI   PBT,C' '                                                         
         BNH   DMXINIT2                                                         
         PACK  DUB,PBT                                                          
         CVB   RE,DUB                                                           
         STC   RE,SELBT                                                         
*                                                                               
DMXINIT2 CLI   PAUL,C' '                                                        
         BNH   *+10                                                             
         MVC   SELAUL,PAUL                                                      
*                                                                               
         CLI   PCUL,C' '                                                        
         BNH   *+10                                                             
         MVC   SELCUL,PCUL                                                      
*                                                                               
         CLI   PWO,C' '                                                         
         BNH   *+10                                                             
         MVC   SELWO,PWO                                                        
*                                                                               
         CLI   PREF,C' '                                                        
         BNH   *+10                                                             
         MVC   SELREF,PREF                                                      
*                                                                               
         CLI   PSTART,C' '                                                      
         BNH   DMXINIT4                                                         
         GOTO1 =V(DATCON),DMCB,(0,PSTART),(1,SELSTR)                            
*                                                                               
DMXINIT4 CLI   PEND,C' '                                                        
         BNH   DMXINIT6                                                         
         GOTO1 =V(DATCON),DMCB,(0,PEND),(1,SELEND)                              
*                                                                               
DMXINIT6 CLI   PADD,C' '                                                        
         BNH   DMXINIT8                                                         
         GOTO1 =V(DATCON),DMCB,(0,PADD),(2,SELADD)                              
*                                                                               
DMXINIT8 CLI   PBIL,C' '                                                        
         BE    DMXINITA                                                         
         MVC   SELBIL,PBIL                                                      
*                                                                               
DMXINITA BAS   RE,PRNTHEAD                                                      
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
         USING TRNRECD,R3                                                       
DMXREC   L     R3,AREC                                                          
         GOTO1 RECTYP,DMCB,(C'D',TRNRECD)                                       
         CLI   0(R1),ACRTTRN                                                    
         BNE   DMXKEEP                                                          
         CLI   COMPANY,0                                                        
         BE    DMXREC00                                                         
         CLC   TRNKCPY,COMPANY     SAME COMPANY?                                
         BE    DMXREC00            YES                                          
         CP    COMRECS,=P'0'       ANY RECORDS?                                 
         BE    DMXREC00            NO, SKIP TOTALS                              
         LA    R6,COMRECS          NO, PRINT TOTALS                             
         BAS   RE,PRNTOTS                                                       
         ZAP   COMRECS,=P'0'                                                    
*                                                                               
DMXREC00 MVC   COMPANY,TRNKCPY                                                  
*                                                                               
         CLI   SELAUL,C' '                                                      
         BNH   DMXREC0                                                          
         CLC   SELAUL,TRNKUNT                                                   
         BNE   DMXKEEP                                                          
*                                                                               
DMXREC0  CLI   SELCUL,C' '                                                      
         BNH   DMXREC2                                                          
         CLC   SELCUL,TRNKCUNT                                                  
         BNE   DMXKEEP                                                          
*                                                                               
DMXREC2  CLI   SELWO,C' '                                                       
         BNH   DMXREC4                                                          
         CLC   SELWO,TRNKWORK                                                   
         BNE   DMXKEEP                                                          
*                                                                               
DMXREC4  CLI   SELREF,C' '                                                      
         BNH   DMXREC6                                                          
         CLC   SELREF,TRNKREF                                                   
         BNE   DMXKEEP                                                          
*                                                                               
DMXREC6  CLI   SELSTR,0                                                         
         BNH   DMXREC8                                                          
         CLC   TRNKDATE,SELSTR                                                  
         BL    DMXKEEP                                                          
*                                                                               
DMXREC8  CLC   TRNKDATE,SELEND                                                  
         BH    DMXKEEP                                                          
*                                                                               
         USING TRNELD,R2                                                        
         LA    R2,TRNRFST          ALWAYS GET 44 AND 60 FOR PRINTING            
         CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                MUST BE FIRST ELEMENT                        
*                                                                               
         CLI   SELBT,0                                                          
         BNH   DMXREC10                                                         
         CLC   SELBT,TRNTYPE       MATCH TYPE IF ENTERED                        
         BNE   DMXKEEP                                                          
*                                                                               
         USING TRSELD,R4                                                        
DMXREC10 LA    R4,TRNRFST                                                       
*                                                                               
DMXREC12 CLI   TRSEL,0                                                          
         BE    DMXKEEP                                                          
         CLI   TRSEL,TRSELQ                                                     
         BE    DMXREC14                                                         
*                                                                               
         SR    R0,R0               GET NEXT ELEMENT                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DMXREC12                                                         
*                                                                               
DMXREC14 CLI   SELADD,0                                                         
         BNH   DMXREC16                                                         
         CLC   SELADD,TRSDATE                                                   
         BNE   DMXKEEP                                                          
*                                                                               
DMXREC16 ZAP   ITMAMNT,TRNAMNT                                                  
         CLI   SELBIL,0                                                         
         BE    DMXREC24                                                         
*                                                                               
         LA    R4,TRNRFST                                                       
*                                                                               
         USING PTAELD,R4                                                        
DMXREC18 CLI   PTAEL,0                                                          
         BE    DMXKEEP                                                          
         CLI   PTAEL,PTAELQ                                                     
         BE    DMXREC22                                                         
*                                                                               
DMXREC20 SR    R0,R0               GET NEXT ELEMENT                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DMXREC18                                                         
*                                                                               
DMXREC22 CLC   SELBIL,PTARBLNO                                                  
         BNE   DMXREC20                                                         
         ZAP   ITMAMNT,PTANET                                                   
*                                                                               
DMXREC24 BAS   RE,PRNTDET                                                       
         B     DMXKEEP                                                          
         EJECT                                                                  
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
*                                                                               
DMXRET   DS    0H                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         CP    COMRECS,=P'0'       DID WE PRINT COMPANY TOTAL YET?              
         BE    *+12                                                             
         LA    R6,COMRECS                                                       
         BAS   RE,PRNTOTS                                                       
         LA    R6,TOTRECS                                                       
         BAS   RE,PRNTOTS                                                       
         B     DMXIT                                                            
         EJECT                                                                  
DMPGET   NTR1  ,                                                                
         L     R9,AREC                                                          
         LA    R7,=C'GET'                                                       
         MVC   HALF,ACCRLEN-ACCRECD(R9)                                         
         LH    R8,HALF                                                          
         GOTO1 VPRNTBL,DMCB,(3,(R7)),AREC,C'DUMP',(R8),=C'2D'                   
         XIT1  ,                                                                
         EJECT                                                                  
         USING PRNTD,R7                                                         
PRNTHEAD NTR1                                                                   
         LA    R7,P                                                             
         MVC   PHCPY,=CL2'CO'                                                   
         MVC   PHACC,=CL14'ACCCOUNT CODE '                                      
         MVC   PHWRK,=CL2'WC'                                                   
         MVC   PHCON,=CL14'CONTRA ACCOUNT'                                      
         MVC   PHREF,=CL6'REFER '                                               
         MVC   PHTYP,=CL2'TY'                                                   
         MVC   PHDAT,=CL6' DATE '                                               
         MVC   PHBAT,=CL6'BATCH '                                               
         MVC   PHADD,=CL6'ADDED '                                               
         MVC   PHAMT,=CL15' DOLLAR AMOUNT '                                     
         GOTO1 VPRINTER                                                         
         XIT1  ,                                                                
         EJECT                                                                  
         USING TRNELD,R2                                                        
         USING TRNRECD,R3                                                       
         USING TRSELD,R4                                                        
         USING PRNTD,R7                                                         
PRNTDET  NTR1                                                                   
         LA    R7,P                                                             
         L     R3,AREC                                                          
         GOTO1 HEXOUT,DMCB,TRNKCPY,PHCPY,L'TRNKCPY                              
         MVC   PACC,TRNKULA                                                     
         MVC   PWRKCD,TRNKOFF                                                   
         MVC   PCONTRA,TRNKULC                                                  
         MVC   PREFER,TRNREF                                                    
         GOTO1 =V(DATCON),DMCB,(1,TRNDATE),(0,PDATE)                            
         GOTO1 =V(DATCON),DMCB,(2,TRSDATE),(0,PADDED)                           
         MVC   PBATCH,TRNBREF                                                   
         EDIT  (B1,TRNTYPE),(2,PTYPE)                                           
         EDIT  ITMAMNT,(15,PAMOUNT),2,CR=YES                                    
         GOTO1 VPRINTER                                                         
         AP    COMRECS,=P'1'                                                    
         AP    TOTRECS,=P'1'                                                    
         AP    TOTAMNT,ITMAMNT                                                  
         XIT1  ,                                                                
         EJECT                                                                  
PRNTOTS  NTR1                                                                   
         MVI   P,C' '                                                           
         GOTO1 VPRINTER                                                         
         MVC   P(17),=C'TOTAL FOR COMPANY'                                      
         GOTO1 HEXOUT,DMCB,COMPANY,P+18,L'TRNKCPY                               
         EDIT  TOTAMNT,(15,P+43),2,CR=YES                                       
         C     R6,=A(COMRECS)                                                   
         BE    PRNTOT2                                                          
         MVC   P(29),=C'GRAND TOTAL FOR ALL COMPANIES'                          
*                                                                               
PRNTOT2  EDIT  (P6,0(R6)),(10,P+30)                                             
         GOTO1 VPRINTER                                                         
         ZAP   LINE,=P'99'                                                      
         BAS   RE,PRNTHEAD                                                      
         XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
VPRNTBL  DC    V(PRNTBL)                                                        
RECTYP   DC    V(ACRECTYP)                                                      
HEXOUT   DC    V(HEXOUT)                                                        
TOTRECS  DC    PL6'0'                                                           
COMRECS  DC    PL6'0'                                                           
TOTAMNT  DC    PL6'0'                                                           
ITMAMNT  DC    PL6'0'                                                           
COMPANY  DS    XL1'00'                                                          
*                                                                               
SELFLDS  DS    0XL(SELLNQ)                                                      
SELBT    DC    XL1'00'                                                          
SELAUL   DC    CL2' '                                                           
SELCUL   DC    CL2' '                                                           
SELWO    DC    CL2' '                                                           
SELREF   DC    CL6' '                                                           
SELSTR   DC    XL3'00'                                                          
SELEND   DC    XL3'FFFFFF'                                                      
SELADD   DC    XL2'00'                                                          
SELBIL   DC    CL6' '                                                           
SELLNQ   EQU   *-SELBT                                                          
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
HALF     DS    H                                                                
WORK     DS    CL20                                                             
*                                                                               
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
*------------------------------------------------------------------*            
*        DSECT FOR PARAMC                                          *            
*------------------------------------------------------------------*            
PARAMD   DSECT                                                                  
PBT      DS    CL2                 BATCH TYPE                                   
PAUL     DS    CL2                 ACCOUNT UNIT/LEDGER                          
PCUL     DS    CL2                 CONTRA UNIT/LEDGER                           
PWO      DS    CL2                 WORKCODE/OFFICE                              
PREF     DS    CL6                 REFERENCE                                    
PSTART   DS    CL6                 START DATE                                   
PEND     DS    CL6                 END DATE                                     
PADD     DS    CL6                 ADD DATE                                     
PBIL     DS    CL6                 BILL NUMBER                                  
PLNQ     EQU   *-PBT                                                            
         EJECT                                                                  
*------------------------------------------------------------------*            
*        DSECT FOR PRINT LINE -                                    *            
*------------------------------------------------------------------*            
PRNTD    DSECT                                                                  
PRNTLNE  DS    CL132                                                            
         ORG   PRNTLNE                                                          
         DS    CL1                                                              
PCPY     DS    XL1                                                              
         DS    CL1                                                              
PACC     DS    CL14                                                             
         DS    CL1                                                              
PWRKCD   DS    CL2                                                              
         DS    CL1                                                              
PCONTRA  DS    CL14                                                             
         DS    CL1                                                              
PREFER   DS    CL6                                                              
         DS    CL1                                                              
PTYPE    DS    CL2                                                              
         DS    CL1                                                              
PDATE    DS    CL6                                                              
         DS    CL1                                                              
PBATCH   DS    CL6                                                              
         DS    CL1                                                              
PADDED   DS    CL6                                                              
         DS    CL1                                                              
PAMOUNT  DS    CL15                                                             
         ORG   PRNTLNE                                                          
PHCPY    DS    CL2                                                              
         DS    CL1                                                              
PHACC    DS    CL14                                                             
         DS    CL1                                                              
PHWRK    DS    CL2                                                              
         DS    CL1                                                              
PHCON    DS    CL14                                                             
         DS    CL1                                                              
PHREF    DS    CL6                                                              
         DS    CL1                                                              
PHTYP    DS    CL2                                                              
         DS    CL1                                                              
PHDAT    DS    CL6                                                              
         DS    CL1                                                              
PHBAT    DS    CL6                                                              
         DS    CL1                                                              
PHADD    DS    CL6                                                              
         DS    CL1                                                              
PHAMT    DS    CL15                                                             
         ORG                                                                    
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
* ACOPTEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACOPTEQUS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACLDXTYP  05/20/02'                                      
         END                                                                    
