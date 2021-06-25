*          DATA SET PPREP0802  AT LEVEL 116 AS OF 05/01/02                      
*PHASE PP0802A,+0,NOAUTO                                                        
*                                                                               
*        THIS PROGRAM WILL FIND AND DELETE I/O'S                                
*        PRODUCED ON DATE IN QSTART                                             
*                                                                               
*        AND PRODUCED ON DATE IN QEND (OPTIONAL)                                
*                                                                               
*****                                                                           
*****    NOTE: MANUAL AND $INSOR I/O'S ARE NOT PURGED                           
*****                                                                           
*                                                                               
*        PUT AGY/MED IN QAGENCY AND QMEDIA                                      
*        PUT CLIENT IN QCLIENT  (OPTIONAL)                                      
*        PUT PRD IN QPRODUCT    (OPTIONAL)                                      
*                                                                               
*        PUT DATE IN QSTART                                                     
*        PUT DUPLICATE IN QEND (OPTIONAL)                                       
*                                                                               
*        QOPT4 T= TURNAROUNDS ONLY                                              
*              X= DON'T PURGE T/A'S                                             
*                                                                               
*        QOPT5 N=DON'T MARK FILE                                                
*              Y=LIVE RUN                                                       
*                                                                               
*        QOPT6 D=PDUMP RECS BEFORE AND AFTER                                    
*              Y=DUMP KEY AND ELEM ONLY                                         
*                                                                               
*                                                                               
*INCLUDE MININAM                                                                
         TITLE 'PP0802 - TURNAROUND I/O PURGE'                                  
         PRINT NOGEN                                                            
PP0802   CSECT                                                                  
         NMOD1 0,PP0802                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP08WRKD,R8                                                      
**                                                                              
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   BAD70,=P'0'     BAD INSERTION ORDER ELEMS                        
         ZAP   BADRECS,=P'0'                                                    
         ZAP   INCNT,=P'0'                                                      
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 DATCON,DMCB,(0,QSTART),(3,MYDATE)                                
         XC    MYFDATE,MYFDATE                                                  
         CLI   QEND,C' '                                                        
         BE    PROC5                                                            
         GOTO1 DATCON,DMCB,(0,QEND),(3,MYFDATE)                                 
*                                                                               
PROC5    CLI   QOPT5,C'N'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY                                                   
         MVI   KEY+3,X'20'         BUYS                                         
         MVC   KEY+4(3),QCLIENT                                                 
         MVC   KEY+7(3),QPRODUCT                                                
AGYC2    GOTO1 HIGH                                                             
         B     AGYC4                                                            
*                                                                               
AGYC3    DS    0H                                                               
         GOTO1 SEQ                                                              
AGYC4    DS    0H                                                               
         CLC   KEY(4),KEYSAVE      SEE IF AT END OF AGY/MED                     
         BNE   EXIT                                                             
         CLI   QPRODUCT,C' '       SEE IF PRODUCT GIVEN                         
         BE    AGYC5                                                            
         CLC   KEY(10),KEYSAVE                                                  
         BNE   EXIT                                                             
*                                                                               
AGYC5    CLI   QCLIENT,C' '        SEE IF CLIENT GIVEN                          
         BE    AGYC6                                                            
         CLC   KEY(7),KEYSAVE                                                   
         BNE   EXIT                                                             
*                                                                               
*                                                                               
AGYC6    DS    0H                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         AP    INCNT,=P'1'                                                      
         MVI   FSW,0                                                            
         LA    R2,PBDELEM                                                       
         MVI   ELCODE,X'70'                                                     
AGYC6C   BAS   RE,NEXTEL                                                        
         BNE   AGYC6X                 SKIP THIS BUY                             
         USING PIOELEM,R2                                                       
         CLI   PIOTURN,C'M'           MANUALS - ALWAYS SKIP                     
         BE    AGYC6C                                                           
         CLI   PIOTURN,0              FROM $INSOR - ALWAYS SKIP                 
         BE    AGYC6C                                                           
         CLI   QOPT4,C'X'             SEE IF EXCLUDING T/A'S                    
         BNE   AGYC6D                                                           
         CLI   PIOTURN,C'T'                                                     
         BE    AGYC6C                                                           
         B     AGYC6E                                                           
*                                                                               
AGYC6D   CLI   QOPT4,C'T'             SEE IF DOING ONLY T/A                     
         BNE   AGYC6E                                                           
         CLI   PIOTURN,C'T'           MUST BE TURNAROUND                        
         BNE   AGYC6C                                                           
AGYC6E   OC    MYFDATE,MYFDATE        SEE IF FIRST DATE GIVEN                   
         BZ    AGYC6F                                                           
         CLC   PIODATE,MYFDATE                                                  
         BNE   AGYC6C                                                           
         MVI   FSW,X'01'                                                        
         B     AGYC6C                                                           
*                                                                               
AGYC6F   CLC   PIODATE,MYDATE                                                   
         BNE   AGYC6C                                                           
         OC    MYFDATE,MYFDATE         SEE IF FIRST DATE GIVEN                  
         BZ    AGYC6G                                                           
         CLI   FSW,X'01'                                                        
         BE    AGYC6G             MEANS WASN'T PRODUCED ON FIRST DATE           
         MVC   P+1(26),=C'** NO I/O ON FIRST DATE **'                           
         MVC   PSECOND+1(21),=C'THIS I/O STILL PURGED'                          
         BAS   RE,RPRT                                                          
*                                                                               
AGYC6G   BAS   RE,AGYCERR                                                       
         CLI   QOPT6,C'D'          SEE IF DUMPING                               
         BNE   AGYC6G5                                                          
         MVC   P+1(6),=C'BEFORE'                                                
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
AGYC6G5  GOTO1 RECUP,DMCB,(1,PBUYREC),(R2),0                                    
         CLI   QOPT6,C'D'                                                       
         BNE   AGYC6G9                                                          
         MVC   P+1(5),=C'AFTER'                                                 
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
AGYC6G9  AP    BAD70,=P'1'                                                      
         AP    BADRECS,=P'1'                                                    
         CLI   RCWRITE,C'Y'                                                     
         BNE   AGYC3                                                            
         GOTO1 PUTPRT                                                           
AGYC6X   B     AGYC3           GO DO NEXT BUY                                   
*                                                                               
         EJECT                                                                  
         DS    F                                                                
AGYCERR  ST    RE,AGYCERR-4                                                     
         MVC   P(3),PBUYKCLT                                                    
         MVC   P+5(3),PBUYKPRD                                                  
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+10(3),DUB                                                      
         GOTO1 PUBEDIT,DMCB,PBUYKPUB,P+16                                       
         GOTO1 DTCNV,DMCB,(1,PBUYKDAT),(4,P+30)                                 
         LA    R4,P+35                                                          
         CLI   PBDFREQ,C'M'                                                     
         BNE   *+14                                                             
         MVC   P+33(2),SPACES                                                   
         LA    R4,P+33                                                          
         CLI   PBUYKLIN,1                                                       
         BE    AGYCE5                                                           
         ZIC   R0,PBUYKLIN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(2,R4),DUB                                                      
         MVI   0(R4),C'-'                                                       
*                                                                               
AGYCE5   DS    0H                                                               
         MVC   HALF,PIONUM                                                      
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+48(4),DUB                                                      
         MVC   P+61(1),PIOTYP                                                   
         MVC   P+68(1),PIOTURN                                                  
         BAS   RE,RPRT                                                          
         BAS   RE,RPRT                                                          
*                                                                               
         CLI   QOPT6,C'Y'                                                       
         BNE   AGYCX                                                            
         MVC   P+1(4),=C'KEY='                                                  
         GOTO1 HEXOUT,DMCB,PBUYREC,P+7,40,=C'N'                                 
         MVC   PSECOND+2(5),=C'ELEM='                                           
         GOTO1 HEXOUT,DMCB,0(R2),PSECOND+9,50,=C'N'                             
         GOTO1 HEXOUT,DMCB,KEY+27,P+95,4,=C'N'                                  
         BAS   RE,RPRT                                                          
AGYCX    L     RE,AGYCERR-4                                                     
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
COUNTS   DS    0C                                                               
INCNT    DS    PL8                                                              
         DC    CL15'BUYRECS READ'                                               
BAD70    DS    PL8                                                              
         DC    CL15'I/O ELEMS PURGED'                                           
BADRECS  DS    PL8                                                              
         DC    CL15'BUYRECS CHANGED'                                            
         DC    X'FF'                                                            
*                                                                               
CNT24    DS    PL4                                                              
CNT70    DS    PL4                                                              
OLDDATE  DS    CL3                                                              
ERRSW    DS    CL1                                                              
MYFDATE  DS    XL3                                                              
MYDATE   DS    XL3                                                              
DDSBLD   DS    CL1                                                              
FSW      DS    CL1                                                              
DATETYP  DS    CL1                                                              
*                                                                               
OUTSPC   DS    CL40                                                             
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL5                                                            
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVC   HEAD2(6),=C'CLIENT'                                              
         MVC   HEAD2+09(3),QCLIENT                                              
         MVC   HEAD3(7),=C'PRODUCT'                                             
         MVC   HEAD3+09(3),QPRODUCT                                             
         MVC   HEAD3+56(9),=C'I/O DATE='                                        
         MVC   HEAD3+66(6),QSTART                                               
         CLI   QEND,C' '                                                        
         BE    RPRT5                                                            
         MVC   HEAD4+56(16),=C'REPEAT I/O DATE='                                
         MVC   HEAD4+73(6),QEND                                                 
*                                                                               
RPRT5    MVC   HEAD5+60(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+66(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                             GET FIRST/NEXT PUB                                
NXTPUB   NTR1                                                                   
         SPACE 2                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),LASTPUB                                                 
         IC    R1,KEY+6                                                         
         LA    R1,1(R1)                                                         
         STC   R1,KEY+6                                                         
         MVC   KEY+7(2),PAGYKAGY                                                
         GOTO1 HIGHPUB                                                          
         B     NP2B                                                             
*                                                                               
NP2      DS    0H                                                               
         GOTO1 SEQPUB                                                           
NP2B     DS    0H                                                               
         MVI   PUBKMED,X'FF'       SET EOF                                      
         CLC   KEY(1),KEYSAVE                                                   
         BNE   NPX                                                              
         CLC   KEY+7(2),KEYSAVE+7                                               
         BNE   NP2                                                              
         CLI   KEY+9,X'81'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LASTPUB,KEY+1                                                    
         GOTO1 GETNAME                                                          
         L     RF,ALTLREC                                                       
         XC    0(50,RF),0(RF)                                                   
         GOTO1 SEQPUB                                                           
         CLC   KEY(9),PUBKEY                                                    
         BNE   NPX                                                              
         GOTO1 GETLTL                                                           
*                                                                               
NPX      DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
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
         L     R5,AREC                                                          
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
         LTORG                                                                  
         SPACE 2                                                                
PP08WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
FRSTSW   DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
ITOT     DS    F                                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'116PPREP0802 05/01/02'                                      
         END                                                                    
