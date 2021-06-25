*          DATA SET ACREPIH02  AT LEVEL 091 AS OF 05/01/02                      
*PHASE ACIH02A                                                                  
*              PROFILES                                                         
*              NONE                                                             
         SPACE 1                                                                
*        QOPT1=Y, PRODUCE OUTPUT TAPE                                           
         TITLE 'H AND K GENERAL LEGDER INTERFACE TAPE'                          
ACIH02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACIH**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACIH02D,RC                                                       
         EJECT                                                                  
         CLI   MODE,RUNFRST                                                     
         BNE   HNK100                                                           
         L     RF,=A(BUFFALOC)                                                  
         ST    RF,ABUFF                                                         
         L     RF,=A(BOXHEAD)                                                   
         ST    RF,HEADHOOK                                                      
         L     RF,=A(BOXRC)                                                     
         ST    RC,0(RF)                                                         
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         L     RF,ADMASTD                                                       
         USING MASTD,RF                                                         
         L     RF,MCBXAREA                                                      
         ST    RF,ADBOX                                                         
         DROP  RF                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
         SPACE 1                                                                
         B     EXIT                                                             
         SPACE 3                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,15,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(31)'                                  
         EJECT                                                                  
HNK100   CLI   MODE,REQFRST                                                     
         BNE   HNK300                                                           
         ZAP   REQDBT,=P'0'                                                     
         ZAP   REQCRT,=P'0'                                                     
         MVC   PAGE,=H'1'                                                       
         L     R2,=A(LDGNAMES)                                                  
         MVI   0(R2),0             INIT LEDGERNAME TABLE                        
         SPACE 1                                                                
         CLI   QOPT1,C'Y'          DO THEY WANT A TAPE                          
         BNE   HNK170                                                           
         LH    R2,OUTCNT                                                        
         LA    R2,1(R2)                                                         
         STH   R2,OUTCNT                                                        
         MVC   DSPARM+13(2),ALPHAID                                             
         GOTO1 DYNALLOC,DMCB,(0,DDPARM),((R2),DSPARM)                           
         OPEN  (TAPEOUT,(OUTPUT))                                               
         ZAP   TPRECS,=P'0'                                                     
*                                                                               
HNK170   B     EXIT                                                             
         EJECT                                                                  
HNK300   CLI   MODE,LEDGFRST                                                    
         BNE   HNK400                                                           
         SPACE 1                                                                
         USING SORTD,R5                                                         
         LA    R5,SORTREC                                                       
         MVC   SORTREC(SRKEYLN),SPACES                                          
         ZAP   SRCR,=P'0'                                                       
         ZAP   SRDR,=P'0'                                                       
         SPACE 1                                                                
         L     R2,ADLEDGER                                                      
         MVC   LEDGCD,1(R2)        SAVE UNIT/LEDGER                             
*        CLC   1(2,R2),=C'S9'      G/L POSTING CONTROL                          
*        BE    HNKEXT                                                           
         SPACE 1                                                                
         MVC   WORK,SPACES        SAVE LEDGER/NAME IN TABLE                     
         MVC   WORK(2),1(R2)                                                    
         USING ACNAMED,R2                                                       
         L     R2,ADLDGNAM                                                      
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+2(0),ACNMNAME                                               
         L     R2,=A(LDGNAMES)                                                  
         ZIC   R1,0(R2)            NUMBER IN TABLE                              
         ZIC   R0,1(R2)            MAX                                          
         CR    R0,R1               STILL ROOM IN TABLE                          
         BH    *+6                 YES                                          
         DC    H'0'                                                             
         LR    R3,R1               NUMBER IN TABLE TO R3                        
         LA    R0,LDGTABLN         LENGTH OF TABLE                              
         STH   R0,HALF                                                          
         MH    R3,HALF             TIMES NUMBER IS OFFSET TO NEXT FREE          
         LA    R2,2(R2)                                                         
         AR    R2,R3                                                            
         MVC   0(LDGTABLN,R2),WORK SAVE THE LEDGER AND NAME                     
         L     R2,=A(LDGNAMES)     BUMP NUMBER IN TABLE                         
         LA    R1,1(R1)                                                         
         STC   R1,0(R2)                                                         
         SPACE 1                                                                
         MVC   LDGGLAC,SPACES  '                                                
         L     R2,ADLDGEL                                                       
         LA    R3,LDGGLAC                                                       
         LA    R4,LDGOFF                                                        
         BAS   RE,GETACCT          GET GL ACCOUNT DATA                          
         CLC   LDGGLAC,SPACES                                                   
         BNE   *+10                                                             
         MVC   LDGGLAC(4),=C'NONE'                                              
         SPACE 1                                                                
         B     EXIT                                                             
         EJECT                                                                  
HNK400   CLI   MODE,PROCACC                                                     
         BNE   HNK500                                                           
         USING SORTD,R5                                                         
         LA    R5,SORTREC                                                       
*        MVI   FCRDTRNS,C'Y'                                                    
*        CLC   LEDGCD(2),=C'S9'                                                 
*        BNE   HNK405              NO ACCOUNTS HAVE POSTING INSTR.              
*        MVI   FCRDTRNS,C'N'                                                    
*        B     EXIT                                                             
         SPACE 1                                                                
HNK405   MVC   SRFROM,SPACES       INIT SORT FIELDS INCASE NO GLACC             
         MVC   SRFROM(2),LEDGCD                                                 
         MVC   SROFF,=C'00'                                                     
         MVC   SRGLACC,LDGGLAC                                                  
         MVC   BYOFF,LDGOFF                                                     
         SPACE 1                                                                
         L     R2,ADACC                                                         
         AH    R2,DATADISP                                                      
         MVC   ACCGLAC,SPACES                                                   
         LA    R3,ACCGLAC          PUT FROM ACCOUNT IN SORT REC                 
         LA    R4,ACCOFF                                                        
         BAS   RE,GETACCT                                                       
         CLC   ACCGLAC,SPACES      DID I FIND AN ACCOUNT HERE                   
         BE    HNK420              NO                                           
HNK410   MVC   SRGLACC,ACCGLAC                                                  
         MVC   BYOFF,ACCOFF                                                     
         L     R2,ADACC                                                         
         MVC   SRFROM,1(R2)                                                     
         SPACE 1                                                                
HNK420   CLC   LEDGCD(2),=C'SI'  IS THIS INCOME LEDGER                          
         BNE   HNK450                                                           
         CLI   BYOFF,C'Y'         POSTING BY OFFICE                             
         BNE   HNK460                                                           
         L     R2,ADACC                                                         
         MVC   SROFF,4(R2)         OFFICE IS IN KEY OF SI ACCOUNTS              
         MVI   BYOFF,C'N'         TURN OFF SO PROCTRNS DOSENT GET               
         SPACE 1                                                                
HNK450   CLC   LEDGCD(2),=C'SJ'    PROD INVENTORY HAS DIFFRENT                  
         BNE   HNK460                                                           
         CLI   BYOFF,C'Y'         POSTING BY OFFICE                             
         BNE   HNK460                                                           
         SPACE 1                                                                
*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-         
*        DOES ACMASTER CALL GETOPT FOR  SJ IN AN ALL LEDGER REQ                 
*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-         
         USING GETOPTD,R4                                                       
         L     R4,ADGOBLOC                                                      
         MVC   SROFF,GOEFFOFC   OFFICE CODE FOR PRODUCTION                      
         MVI   BYOFF,C'N'         TURN OFF SO PROCTRNS DOSENT GET               
HNK460   B     EXIT                                                             
         EJECT                                                                  
HNK500   CLI   MODE,PROCTRNS                                                    
         BNE   HNK600                                                           
         SPACE 1                                                                
         LA    R5,SORTREC                                                       
         L     R3,ADTRANS                                                       
         USING TRANSD,R3                                                        
         CLI   TRNSEL,X'44'                                                     
         BNE   HNKEXT                                                           
         LR    R6,R3                                                            
         SH    R6,DATADISP                                                      
         USING ACKEYD,R6                                                        
         OC    ACDTPEEL,ACDTPEEL                                                
         BNZ   EXIT                IGNORE PEELED TRANSACTIONS                   
         USING ACMD,R2                                                          
         L     R2,AMONACC          PUT MOS TO SORT                              
         GOTO1 HEXOUT,DMCB,ACMMDTE,SRMOS,4                                      
         SPACE 1                                                                
         CLI   BYOFF,C'Y'         OFFICE IN SORT RECORD                         
         BNE   HNK530                                                           
         SPACE 1                                                                
         MVC   SROFF,TRNSANAL     OFFICE CODE                                   
         SPACE 1                                                                
HNK530   ZAP   SRDR,=P'0'                                                       
         ZAP   SRCR,=P'0'                                                       
         SPACE 1                                                                
         LA    R8,SRDR                                                          
         TM    TRNSSTAT,X'80'                                                   
         BO    HNK540                                                           
         LA    R8,8(R8)            ADD TO CREDITS                               
HNK540   AP    0(8,R8),TRNSAMNT                                                 
         SPACE 1                                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,SORTREC                               
         B     EXIT                                                             
         EJECT                                                                  
HNK600   CLI   MODE,ACCLAST                                                     
         BNE   HNK700                                                           
         SPACE 1                                                                
         EJECT                                                                  
HNK700   CLI   MODE,LEDGLAST                                                    
         BNE   HNK800                                                           
         EJECT                                                                  
HNK800   CLI   MODE,REQLAST                                                     
         BNE   HNK1000                                                          
         USING SORTD,R5                                                         
         LA    R5,SORTREC                                                       
         MVC   SORTREC(SRKEYLN),SPACES                                          
         ZAP   SRCR,=P'0'                                                       
         ZAP   SRDR,=P'0'                                                       
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,SORTREC,1                            
HNK830   TM    DMCB+8,X'80'                                                     
         BO    HNK900                                                           
         AP    REQCRT,SRCR                                                      
         AP    REQDBT,SRDR                                                      
         USING PLINED,R4                                                        
         LA    R4,P                                                             
         MVC   POFF,SROFF                                                       
         MVC   PGLAC,SRGLACC                                                    
         MVC   PFROM,SRFROM                                                     
         LA    R2,SRDR                                                          
         LA    R3,PDR                                                           
         BAS   RE,PRTBUCK                                                       
         LA    R2,SRCR                                                          
         LA    R3,PCR                                                           
         BAS   RE,PRTBUCK                                                       
         ZAP   DOUBLE,SRDR                                                      
         SP    DOUBLE,SRCR                                                      
         LA    R2,DOUBLE                                                        
         LA    R3,PBAL                                                          
         BAS   RE,PRTBUCK                                                       
         GOTO1 ACREPORT                                                         
         SPACE 1                                                                
         CLI   QOPT1,C'Y'                                                       
         BNE   HNK870                                                           
         USING TAPED,R4                                                         
         LA    R4,TAPEREC                                                       
         MVC   TPREC(TPRECLN),SPACES                                            
         MVC   TPREC(SRKEYLN),SORTREC  BEGINING OF TAPEREC IS LIKE SORT         
         L     R2,=A(LDGNAMES)                                                  
         ZIC   R1,0(R2)            NUMBER IN TABLE                              
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO LEDGERS?                                  
         LA    R2,2(R2)            POINT TO DATA                                
HNK845   CLC   0(2,R2),SRFROM      FIND LEDGER THIS POSTING CAME FROM           
         BE    HNK850                                                           
         LA    R2,LDGTABLN(R2)     NEXT TABLE ENTRY                             
         BCT   R1,HNK845                                                        
         DC    H'0'                LEDGER NOT FOUND?                            
         SPACE 1                                                                
HNK850   MVC   TPLEDGER,2(R2)      WRITE LEDGER NAME TO TAPE                    
         UNPK  TPDR,SRDR                                                        
         UNPK  TPCR,SRCR                                                        
         SPACE 1                                                                
         PUT   TAPEOUT,TAPEREC                                                  
         AP    TPRECS,=P'1'                                                     
HNK870   GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,SORTREC,1                             
         B     HNK830                                                           
         EJECT                                                                  
HNK900   MVC   P+1(6),=C'TOTALS'                                                
         LA    R4,P                                                             
         USING PLINED,R4                                                        
         LA    R2,REQDBT                                                        
         LA    R3,PDR                                                           
         BAS   RE,PRTBUCK                                                       
         LA    R2,REQCRT                                                        
         LA    R3,PCR                                                           
         BAS   RE,PRTBUCK                                                       
         ZAP   DOUBLE,REQDBT                                                    
         SP    DOUBLE,REQCRT                                                    
         LA    R2,DOUBLE                                                        
         LA    R3,PBAL                                                          
         BAS   RE,PRTBUCK                                                       
         GOTO1 ACREPORT                                                         
HNK1000  B     HNKEXT                                                           
         SPACE 2                                                                
HNKEXT   XIT1                                                                   
         EJECT                                                                  
         GETEL R2,DATADISP,ELCODE                                               
         SPACE 1                                                                
PRTBUCK  CP    0(8,R2),=P'0'                                                    
         BER   RE                                                               
         EDIT  (P8,0(R2)),(15,0(R3)),2,MINUS=YES                                
         BR    RE                                                               
         SPACE 1                                                                
         EJECT                                                                  
*              PRINT A SUMMARY                                                  
         SPACE 2                                                                
*                                  PARA 1 = BUFFALO LEVEL                       
         EJECT                                                                  
         SPACE 1                                                                
CTXIT    XMOD1 1                                                                
EXIT     XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        GET THE "GEN LEDG POSTING ACCOUNT FROM A 15 EL                         
*        R2 IS A(RECORD)  RETIURNS ACCOUNT IN 0(R3), 0(R4)=Y IF BYOFF           
*----------------------------------------------------------------------         
GETACCT  NTR1                                                                   
         MVI   0(R4),C' '                                                       
         SPACE 1                                                                
         USING ACGENLD,R2                                                       
GETA005  CLI   0(R2),0                                                          
         BE    EXIT                                                             
         CLI   0(R2),X'15'                                                      
         BE    GETA010                                                          
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     GETA005                                                          
         SPACE 1                                                                
GETA010  LA    R5,ACGLACC                                                       
         CLI   0(R5),C'*'          DOES H&K WANT OFFICE TOTALS HERE             
         BNE   GETA030             NO                                           
         MVI   0(R4),C'Y'                                                       
         LA    R5,1(R5)            BYPSS *                                      
         SPACE 1                                                                
GETA030  MVC   0(9,R3),0(R5)                                                    
         B     EXIT                                                             
         EJECT                                                                  
*              CONSTANTS, LITERAL POOL                                          
         SPACE 1                                                                
*              EQUATES FOR BYPASS SWITCH                                        
         SPACE 1                                                                
BYLEDGER EQU   1                                                                
BYACCNT  EQU   2                                                                
         SPACE 1                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*                             DSECT IS PSTBD                                    
         SPACE 2                                                                
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,RECFM=FB,LRECL=089,BLKSIZE=8900,X        
               MACRF=PM                                                         
         SPACE 2                                                                
DDPARM   DC    CL8'TAPEOUT'                                                     
DSPARM   DC    CL20'ACCTAPE.AC0IHXX1'                                           
OUTCNT   DC    H'0'                                                             
         SPACE 2                                                                
         BUFF  LINES=1000,ROWS=1,COLUMNS=2,FLAVOR=PACKED,              X        
               KEYLIST=(29,A)                                                   
         SPACE 2                                                                
LDGTABLN EQU   38                  2+36 FOR THE NAME                            
LDGTABMX EQU   35                                                               
LDGNAMES DC    AL1(0)                                                           
         DC    AL1(LDGTABMX)                                                    
         DS    CL(LDGTABMX*LDGTABLN)                                            
         EJECT                                                                  
BOXHEAD  NMOD1 0,*BOXHK                                                         
         L     RC,BOXRC                                                         
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         SPACE 1                                                                
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+7,C'C'                                                   
         MVI   BOXCOLS+17,C'C'                                                  
         MVI   BOXCOLS+32,C'C'                                                  
         MVI   BOXCOLS+48,C'C'                                                  
         MVI   BOXCOLS+64,C'C'                                                  
         MVI   BOXCOLS+80,C'R'                                                  
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
BOXX     XIT1                                                                   
         SPACE 1                                                                
BOXRC    DS    A                                                                
*              DSECTS                                                           
         SPACE 2                                                                
TAPED    DSECT                     DSECT FOR TAPE                               
TPREC    DS    0C                                                               
TPOFF    DS    CL2                 OFFICE OR C'00'                              
TPGLACC  DS    CL9                 H&K GL ACCOUNT                               
TPFROM   DS    CL14                DDS FROM ACCOUNT                             
TPMOS    DS    CL4                 MOS OF POSTING                               
TPLEDGER DS    CL36                NAME OF FROM LEDGER                          
TPDR     DS    CL12                DEBITS                                       
TPCR     DS    CL12                CREDITS                                      
TPRECLN  EQU   *-TAPED                                                          
         SPACE 2                                                                
SORTD    DSECT                     DSECT FOR TAPE                               
SROFF    DS    CL2                 OFFICE OR C'00'                              
SRGLACC  DS    CL9                 H&K GL ACCOUNT                               
SRFROM   DS    CL14                DDS FROM ACCOUNT                             
SRMOS    DS    CL4                 MOS OF POSTING                               
SRKEYLN  EQU   *-SORTD                                                          
SRDR     DS    PL8                 DEBITS                                       
SRCR     DS    PL8                 CREDITS                                      
SRRECLN  EQU   *-SORTD                                                          
         SPACE 2                                                                
         SPACE 2                                                                
*              PLINED,DSECT FOR PRINT LINE                                      
         SPACE 2                                                                
PLINED   DSECT                                                                  
PLINE    DS    0CL110                                                           
         DS    CL1                                                              
POFF     DS    CL2                 OF                                           
         DS    CL4                 FICE                                         
         DS    CL1                 +7                                           
PGLAC    DS    CL9                 ACCOUNT                                      
         DS    CL1                                                              
PFROM    DS    CL14                ORIGIN                                       
         DS    CL1                                                              
PDR      DS    CL15                DEBIT                                        
         DS    CL1                                                              
PCR      DS    CL15                CREDIT                                       
         DS    CL1                                                              
PBAL     DS    CL15                BALANCE                                      
         DS    CL1                                                              
         SPACE 1                                                                
         ORG   PLINE                                                            
         DS    CL6                                                              
PLTOT    DS    CL10                USUALLY SET TO 'TOTALS FOR'                  
         SPACE 2                                                                
*              LEVGEND,DSECT FOR LEVAGEN ETC. CONTROLS SUBROUTINES SUCH         
*              AS LEVUP & LEVLAST.                                              
         SPACE 1                                                                
         EJECT                                                                  
ACIH02D  DSECT                                                                  
ADBOX    DS    A                                                                
TPRECS   DS    PL8                 NUMBER POSTINGS                              
REQDBT   DS    PL8                 REQ DEBITS                                   
REQCRT   DS    PL8                 REQ CREDITS                                  
         SPACE 1                                                                
ABUFF    DS    A                   A(BUFFALO)                                   
SORTREC  DS    CL(SRRECLN)         SORTER RECORD                                
TAPEREC  DS    CL(TPRECLN)         TAPE RECORD                                  
         SPACE 1                                                                
LEDGCD   DS    CL2                 UNIT/LEDGER                                  
ACCGLAC  DS    CL9                                                              
ACCOFF   DS    CL1                                                              
LDGGLAC  DS    CL9                                                              
LDGOFF   DS    CL1                                                              
ELCODE   DS    CL1                                                              
BYOFF    DS    CL1                                                              
         SPACE 1                                                                
* ACGENBOTH                                                                     
* ACGENPOST                                                                     
* ACREPWORKD                                                                    
* ACGENMODES                                                                    
* DDLOGOD                                                                       
* DDREPXTRAD                                                                    
* DDREPMASTD                                                                    
* DDREMOTED                                                                     
GETOPTD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'091ACREPIH02 05/01/02'                                      
         END                                                                    
