*          DATA SET PPREPRI02  AT LEVEL 025 AS OF 08/27/03                      
*PHASE PPRI02A,+0,NOAUTO                                                        
*                                                                               
         TITLE 'PPRI02  - PRINTPAK I/O REVERSAL PROGRAM'                        
*                                                                               
************  CHANGE LOG  ************                                          
*                                                                               
*  SMYE  2/96   ADDED WARNING FOR I/O DATES GT SELECTED I/O DATE - IF           
*              ANY FOUND, ELEMENTS NOT DELETED EXCEPT VIA QOPT5 BELOW           
*                                                                               
*  SMYE  12/18/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                     
*                                                                               
*                                                                               
*              QPAY(6) = I/O DATE  (ALL= ANY DATE)                              
*              QPUB+1(4) = I/O NUMBER                                           
*              QOPT1 = SOURCE   (T, R, M, I OR BLANK)                           
*              QOPT2 = Y = "DUMP RECORDS"                                       
*              QOPT5 = Y = DELETE ELEM'S. REGARDLESS OF LATER I/O DATES         
*                                                                               
PPRI02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPRI02                                                         
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R8,4095(RC)                                                      
         LA    R8,1(R8)                                                         
         USING PPFILED,RC,R8                                                    
         LA    R9,P                                                             
         USING UBLIND,R9                                                        
         LA    R7,SPACEND                                                       
         USING UBWRKD,R7                                                        
         CLI   MODE,PROCBUY                                                     
         BE    PRBY                                                             
         CLI   MODE,FBUYREQ                                                     
         BE    INIT                                                             
         CLI   MODE,LBUYREQ                                                     
         BE    LBYR                                                             
         CLI   MODE,RUNLAST                                                     
         BE    LAST                                                             
         CLI   MODE,RUNFRST                                                     
         BE    FIRST                                                            
         CLI   MODE,DISKERR                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XIT                                                                    
         EJECT                                                                  
*                                  INITIALIZATION                               
INIT     EQU   *                                                                
         CLC   =C'ALL',QEST                                                     
         BNE   *+10                                                             
         MVC   QEST,SPACES                                                      
         MVI   FCRDACTV,C'N'                                                    
         CLI   QPRODUCT,C' '                                                    
         BE    INITA                                                            
         CLC   QPRODUCT,=C'ALL'                                                 
         BNE   *+8                                                              
INITA    DS    0H                                                               
         MVI   FCRDACTV,C'Y'                                                    
         OI    DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
*                                  RUN DATE                                     
         MVC   TODAY(2),RCDATE+6                                                
         MVC   TODAY+2(2),RCDATE                                                
         MVC   TODAY+4(2),RCDATE+3                                              
*                                                                               
         GOTO1 DATCON,DMCB,(0,TODAY),(3,TODAYB)                                 
*                                                                               
         MVC   SOURCE,QOPT1                                                     
         CLI   SOURCE,C'I'                                                      
         BNE   *+8                                                              
         MVI   SOURCE,0            PIOTURN(SOURCE) = 0 FOR (I)NS                
         CLI   QPUB+1,C' '                                                      
         BE    INIT1                                                            
         PACK  DUB,QPUB+1(4)                                                    
         CVB   R0,DUB                                                           
         STH   R0,MANINV                                                        
         MVC   MANINVC,QPUB+1                                                   
INIT1    CLI   QPUB+5,C' '                                                      
         BE    INIT1C                                                           
         PACK  DUB,QPUB+5(4)                                                    
         CVB   R0,DUB                                                           
         STH   R0,REVINV                                                        
         MVC   REVINVC,QPUB+5                                                   
INIT1C   DS    0H                                                               
*                                  UNIO DATE                                    
         CLI   QPAY,C'A'           ONLY NEEDED ON FIRST REQ                     
         BL    INIT2                                                            
         XC    UDATEB,UDATEB                                                    
         MVC   UDATEP(8),=C'ANY DATE'                                           
         CLC   QPAY(3),=C'ALL'                                                  
         BE    INIT1E                                                           
*                                                                               
*        GOTO1 DTCNV,DMCB,QPAY,(1,UDATEB)                                       
         GOTO1 DATCON,DMCB,(0,QPAY),(3,UDATEB)                                  
*                                                                               
*        GOTO1 DTCNV,DMCB,QPAY,(3,UDATEP)                                       
         GOTO1 DATCON,DMCB,(0,QPAY),(5,UDATEP)                                  
*                                                                               
INIT1E   MVC   UDATE,QPAY                                                       
INIT2    DS    0H                                                               
         MVC   STARTP(16),SPACES                                                
         XC    BSTART,BSTART                                                    
         MVC   BEND,=3X'FF'                                                     
         CLI   QSTART,C'0'                                                      
         BNH   INIT3                                                            
*        GOTO1 DTCNV,DMCB,QSTART,(1,BSTART)                                     
         GOTO1 DATCON,DMCB,(0,QSTART),(3,BSTART)                                
*        GOTO1 (RF),(R1),,(3,STARTP)                                            
         GOTO1 DATCON,(R1),,(5,STARTP)                                          
INIT3    DS    0H                                                               
         CLI   QEND,C'0'                                                        
         BNH   INIT3B                                                           
*        GOTO1 DTCNV,DMCB,QEND,(1,BEND)                                         
         GOTO1 DATCON,DMCB,(0,QEND),(3,BEND)                                    
*        GOTO1 (RF),(R1),,(3,ENDP)                                              
         GOTO1 DATCON,(R1),,(5,ENDP)                                            
INIT3B   DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         XC    IOTOT,IOTOT     CLEAR ACCUMS                                     
*                                                                               
INIT4    DS    0H                                                               
         MVI   QBPDATE,C'B'                                                     
         MVI   TYPE,0                                                           
         B     EXIT                                                             
CRDERR   EQU   *                                                                
         MVC   P(80),QRECORD                                                    
         MVC   P+85(16),=C'BAD CONTROL CARD'                                    
         BAS   RE,RPRT                                                          
         DC    H'0'                                                             
         EJECT                                                                  
*                                  PROCESS BILL                                 
         EJECT                                                                  
*                                  PROCESS BUY                                  
PRBY     EQU   *                                                                
         GOTO1 GETBUY              REREAD BUY TO BE SURE IT WAS                 
*                                  LAST RECORD READ                             
         MVI   BYSW,1              1=OK TO DELETE ELEMENTS                      
         MVI   FRSTSW,1            1=FIRST PASS COMPLETED OR NOT NEEDED         
         MVI   FOUNDSW,1           1=I/O DELETE FOR BUY FOUND                   
         OC    UDATEB,UDATEB       I/O DATE TO TEST?                            
         BZ    PRBY2               NO - FIRST PASS NOT NEEDED                   
         MVI   FRSTSW,0            0=FIRST PASS THRU RECORD NEEDED              
         MVI   FOUNDSW,0                                                        
PRBY2    DS    0H                                                               
         LA    R3,PBUYREC+33                                                    
PRBY4    EQU   *                                                                
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
PRBY4C   CLI   0(R3),X'70'         CHECK FOR I/O ELEM                           
         BE    PRBY6                                                            
*                                                                               
PRBY4E   CLI   0(R3),0                                                          
         BNE   PRBY4                                                            
         B     PRBY10                                                           
**                                                                              
PRBY6    EQU   *                                                                
         USING PIOELEM,R3                                                       
         OC    PIODATE,PIODATE     CHK FOR A DATE                               
         BZ    PRBY4                                                            
*                                                                               
         OC    UDATEB,UDATEB                                                    
         BZ    PRBY6B                                                           
*                                                                               
         CLC   PIODATE,UDATEB                                                   
****     BNE   PRBY4                                                            
         BL    PRBY4               PASS THRU PIODATES = OR GT                   
*                                                                               
PRBY6B   OC    PIONUM,PIONUM       CHK FOR AN I/O NUMBER                        
         BZ    PRBY4                                                            
*                                                                               
         OC    MANINV,MANINV                                                    
         BZ    PRBY6E                                                           
*                                                                               
         CLC   PIONUM(2),MANINV                                                 
         BNE   PRBY4                                                            
*                                                                               
*PRBY6E   CLI   SOURCE,C' '        SOURCE WORKS AS TEST FIELD ONLY              
*        BE    PRBY7B              IF INIT ROUTINE IS CALLED                    
*                                  IN THIS PROGRAM                              
*        CLC   PIOTURN,SOURCE      (PRBY6E BELOW USES QOPT1, USED               
*        BNE   PRBY4                FOR "SOURCE' ON REQUEST CARD)               
*                                                                               
PRBY6E   CLI   QOPT1,C' '                                                       
         BE    PRBY7B                                                           
         CLI   QOPT1,C'I'                                                       
         BNE   *+8                                                              
         MVI   QOPT1,0                                                          
         CLC   PIOTURN,QOPT1                                                    
         BNE   PRBY4                                                            
*                                                                               
PRBY7B   DS    0H                                                               
         CLI   FRSTSW,0            FIRST PASS ?                                 
         BNE   PRBY7E              NO                                           
         CLC   PIODATE,UDATEB                                                   
         BNE   PRBY7C                                                           
         MVI   FOUNDSW,1           EQUAL I/O DATE FOUND                         
         B     PRBY4               NEXT ELEM                                    
PRBY7C   MVI   BYSW,0              "LATER" PIODATE FOUND                        
         B     PRBY4               NEXT ELEM                                    
*                                                                               
PRBY7E   DS    0H                                                               
         CLI   FOUNDSW,1           I/O DELETE FOR BUY FOUND ?                   
         BNE   EXIT                NO                                           
         CLI   TYPE,C'B'                                                        
         BE    *+12                                                             
         MVI   TYPE,C'B'                                                        
         BAS   RE,RPRT                                                          
*                                                                               
         MVC   UBCLT,PBUYKCLT                                                   
         MVC   UBPRD,PBUYKPRD                                                   
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  UBEST,DUB                                                        
*        GOTO1 DTCNV,DMCB,(1,PBUYKDAT),(4,UBYDAT)                               
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(7,UBYDAT)                              
         LA    R4,UBYDAT+5                                                      
         CLI   PBDFREQ,C'M'                                                     
         BNE   *+14                                                             
         MVC   UBYDAT+3(2),SPACES                                               
         LA    R4,UBYDAT+3                                                      
         CLI   PBUYKLIN,1                                                       
         BE    PRBY8                                                            
         SR    R5,R5                                                            
         IC    R5,PBUYKLIN                                                      
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(2,R4),DUB                                                      
         MVI   0(R4),C'-'                                                       
PRBY8    EQU   *                                                                
         GOTO1 PUBEDIT,DMCB,PBUYKPUB,UBPUB                                      
*                                                                               
         EDIT  (B2,PIONUM),(4,P+61),0,FILL=0                                    
         GOTO1 DATCON,DMCB,(3,PIODATE),(8,P+74)                                 
         MVC   P+69(3),=C'NEW'                                                  
         CLI   PIOTYP,C'N'                                                      
         BE    PRBY8X                                                           
         MVC   P+69(3),=C'CHG'                                                  
         CLI   PIOTYP,C'C'                                                      
         BE    PRBY8X                                                           
         MVC   P+69(3),=C'CAN'                                                  
         CLI   PIOTYP,C'D'                                                      
         BE    PRBY8X                                                           
         DC    H'0'               BAD TYPE                                      
PRBY8X   DS    0H                                                               
         MVC   P+84(3),=C'T/A'    AUTO I/O                                      
         CLI   PIOTURN,C'T'                                                     
         BE    PRBY9                                                            
         MVC   P+84(3),=C'REQ'    REQUESTED                                     
         CLI   PIOTURN,C'R'                                                     
         BE    PRBY9                                                            
         MVC   P+84(3),=C'MAN'    MANUAL                                        
         CLI   PIOTURN,C'M'                                                     
         BE    PRBY9                                                            
         MVC   P+84(3),=C'INS'    INSOR                                         
         CLI   PIOTURN,0                                                        
         BE    PRBY9                                                            
         DC    H'0'               BAD TYPE                                      
PRBY9    DS    0H                                                               
         OC    UDATEB,UDATEB       I/O DATE TO TEST??                           
         BZ    PRBY9B              NO                                           
         CLC   PIODATE,UDATEB                                                   
         BE    PRBY9B              GO HANDLE ELEM TO BE DELETED                 
         MVC   P+89(38),=C'*WARNING* - ORDER DATED AFTER I/O DATE'              
         BAS   RE,RPRT             PRINT WARNING MSG LINE                       
         B     PRBY4               GO TEST NEXT ELEM                            
PRBY9B   DS    0H                                                               
         MVC   P(9),=C'*I/O DEL*'                                               
         CLI   QOPT5,C'Y'          Y=DELETE ELEMENTS "REGARDLESS"               
         BNE   *+8                                                              
         MVI   BYSW,1              OK TO DELETE IF QOPT5='Y'                    
         CLI   BYSW,1              OK TO DELETE?                                
         BE    PRBY9C              YES                                          
         MVC   P(9),SPACES                                                      
         MVC   P+89(35),=C'** NOT DELETED **  -  SEE *WARNING*'                 
         BAS   RE,RPRT             PRINT WARNING MSG LINE                       
         B     PRBY4               GO TEST NEXT ELEM                            
PRBY9C   BAS   RE,RPRT                                                          
*                                                                               
         ST    R3,R3SAVE      SAVE POINTER TO ELEM                              
         CLI   QOPT2,C'Y'                                                       
         BNE   PRBY9E                                                           
         MVC   P(14),=C'*** BEFORE ***'                                         
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
PRBY9E   GOTO1 RECUP,DMCB,(1,PBUYREC),(R3),0    ** DELETE ELEM **               
*                                                                               
         L     R0,IOTOT                                                         
         AH    R0,=H'1'                                                         
         ST    R0,IOTOT                                                         
*                                                                               
         CLI   QOPT2,C'Y'                                                       
         BNE   PRBY9X                                                           
         MVC   P(14),=C'*** AFTER  ***'                                         
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
         BAS   RE,RPRT                                                          
PRBY9X   DS    0H                                                               
         B     PRBY4C                                                           
*                                                                               
PRBY10   EQU   *                                                                
         CLI   FRSTSW,0            ANOTHER PASS NEEDED?                         
         BNE   PRBY15              NO                                           
         MVI   FRSTSW,1                                                         
         B     PRBY2                                                            
PRBY15   LA    RF,PBUYREC                                                       
         ST    RF,AREC                                                          
         CLI   RCWRITE,C'Y'                                                     
         BNE   EXIT                                                             
         CLI   BYSW,1              OK TO DELETE?                                
         BNE   EXIT                NO                                           
         GOTO1 PUTPRT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                  LAST BUY FOR REQUEST                         
LBYR     EQU   *                                                                
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         MVI   SPACING,2                                                        
         MVC   P(06),=C'TOTALS'                                                 
         MVC   PSECOND(06),=C'------'                                           
LBYR2    DS    0H                                                               
         BAS   RE,RPRT                                                          
         MVC   P(17),=C'DELETED I/O ELEMS'                                      
         LA    R2,IOTOT                                                         
         LA    R4,P+22                                                          
         BAS   RE,EDIT1                                                         
         BAS   RE,RPRT                                                          
*                                                                               
LBYR2F   DS    0H                                                               
*                                                                               
LBYR2D   DS    0H                                                               
*                                  ROLL TO REPORT TOTALS                        
         L     R0,TIOTOT                                                        
         A     R0,IOTOT                                                         
         ST    R0,TIOTOT                                                        
         XC    IOTOT,IOTOT                                                      
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
LAST     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   IOTOT(4),TIOTOT                                                  
         MVC   P(10),=C'RUN TOTALS'                                             
         MVC   PSECOND(10),=C'----------'                                       
         MVI   SPACING,2                                                        
         MVC   STARTP(16),SPACES                                                
         MVC   UDATEP,SPACES                                                    
         B     LBYR2                                                            
         SPACE 2                                                                
FIRST    DS    0H                                                               
         XC    TIOTOT,TIOTOT        CLEAR RUN TOTALS                            
         B     EXIT                                                             
         SPACE 3                                                                
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVC   HEAD3(6),=C'CLIENT'                                              
         MVC   HEAD4(7),=C'PRODUCT'                                             
         MVC   HEAD5(8),=C'ESTIMATE'                                            
         MVC   HEAD6(9),=C'START-END'                                           
         MVC   HEAD7(8),=C'I/O DATE'                                            
         MVC   HEAD3+10(3),QCLIENT                                              
         MVC   HEAD4+10(3),QPRODUCT                                             
         MVC   HEAD5+10(3),QEST                                                 
         MVC   HEAD6+10(6),QSTART                                               
         MVI   HEAD6+16,C'-'                                                    
         MVC   HEAD6+17(6),QEND                                                 
         MVC   HEAD7+10(6),QPAY                                                 
         CLI   QESTEND,C' '                                                     
         BE    *+14                                                             
         MVI   HEAD5+13,C'-'                                                    
         MVC   HEAD5+14(3),QESTEND                                              
         MVC   HEAD5+47(15),=C'** WRITE=NO  **'                                 
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+56(3),=C'YES'                                              
*                                                                               
*                                                                               
RPRT4    GOTO1 REPORT                                                           
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
EDIT1    EQU   *                                                                
         L     R0,0(R2)                                                         
         EDIT  (R0),(15,0(R4)),0,COMMAS=YES,CR=YES                              
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*******   FORMATTED RECORD DUMPS FOLLOW   ********                              
*                                                                               
DMPKEY   NTR1                                                                   
         SPACE 2                                                                
         LA    R5,KEY                                                           
         LA    R2,25                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R2),=C'N'                                 
*                                                                               
         MVC   WORK(25),0(R5)                                                   
         TR    WORK(25),TRTAB                                                   
         MVC   P+75(25),WORK                                                    
         B     EXIT                                                             
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         LA    R5,PBUYREC          POINT R5 TO BEGINNING OF REC                 
***      L     R5,R3SAVE           POINT R5 TO ELEM BEING PROCESSED             
         LA    R2,220                                                           
         LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,RPRT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         SPACE 3                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         SPACE 3                                                                
ZEROS    DC    30C'0'                                                           
         LTORG                                                                  
         SPACE 3                                                                
UBWRKD   DSECT                                                                  
UBWORK   DS    0F                                                               
IOTOT    DS    F                                                                
TIOTOT   DS    F                                                                
R3SAVE   DS    F                                                                
TODAYB   DS    XL3                                                              
TODAY    DS    CL6                                                              
STARTP   DS    CL8                                                              
ENDP     DS    CL8                                                              
BSTART   DS    XL3                                                              
BEND     DS    CL3                                                              
BYSW     DS    X                   0=DO NOT DELETE ANY ELEMENTS                 
FRSTSW   DS    X                   0=FIRST PASS THRU RECORD                     
FOUNDSW  DS    X                   1=I/O DELETE FOR BUY FOUND                   
UDATE    DS    CL6                                                              
UDATEB   DS    XL3                                                              
UDATEP   DS    CL8                                                              
MYKEY    DS    CL32                                                             
TYPE     DS    X                                                                
SOURCE   DS    X                                                                
MANINV   DS    H                                                                
MANINVC  DS    CL4                                                              
REVINV   DS    H            WHEN UNBILLING REVERSAL MUST SPECIFY                
REVINVC  DS    CL4          REVERSED INVOICE NUMBER                             
*                                                                               
         DS    0F                                                               
RVGROSS  DS    CL12                                                             
UBGROSS  DS    CL12                                                             
         SPACE 3                                                                
         SPACE 3                                                                
UBLIND   DSECT                                                                  
         DS    CL9                                                              
UBCLT    DS    CL3                                                              
         DS    CL2                                                              
UBPRD    DS    CL3                                                              
         DS    CL2                                                              
UBEST    DS    CL3                                                              
         DS    CL2                                                              
UBYDAT   DS    CL8                                                              
         DS    CL2                                                              
UBPUB    DS    CL15                                                             
         DS    CL10                                                             
UBION    DS    CL4              I/O NUMBER                                      
         DS    CL6                                                              
UBTYPE   DS    CL3              I/O TYPE                                        
         DS    CL2                                                              
UBIODT   DS    CL8              I/O DATE                                        
         DS    CL2                                                              
UBIOS    DS    CL3              SOURCE                                          
         DS    CL2                                                              
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025PPREPRI02 08/27/03'                                      
         END                                                                    
