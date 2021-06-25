*          DATA SET PPREPCP02  AT LEVEL 019 AS OF 05/01/02                      
*PHASE PPCP02A,+0,NOAUTO                                                        
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'PPCP02 - OM SPECIAL PRINTPAK CONTRACT PURGE'                    
PPCP02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPCP02                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
         RELOC RELO                                                             
*                                                                               
         LA    R7,PDUMREC                                                       
         USING PPDUM00,R7                                                       
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BNE   EXIT                                                             
*                                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    LASTPUB,LASTPUB                                                  
*                                                                               
*                                  ADD CODE FOR THIS RUN HERE                   
         GOTO1 DATCON,DMCB,QSTART,(3,BQSTART)                                   
         GOTO1 DATCON,DMCB,QEND,(3,BQEND)                                       
         ZAP   PRGCON,=P'0'                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY                                                   
         MVI   KEY+3,X'10'                                                      
         CLC   QCLIENT,=C'ALL'                                                  
         BE    *+10                                                             
         MVC   KEY+4(3),QCLIENT                                                 
GET2     DS    0H                                                               
         BAS   RE,HIGH                                                          
         B     *+8                                                              
GET4     DS    0H                                                               
         BAS   RE,SEQ                                                           
GET4B    DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   END                                                              
         CLC   QCLIENT,=C'ALL'                                                  
         BE    *+14                                                             
         CLC   QCLIENT,KEY+4                                                    
         BNE   END                                                              
*                                                                               
         LA    R0,PCONREC                                                       
         ST    R0,AREC                                                          
         BAS   RE,GETPRT                                                        
         OC    PCONPRD,PCONPRD        ONLY PURGE PRODUCT CONTRACTS              
         BZ    GET4                                                             
*                                                                               
         CLC   PCONSDT,BQSTART     BE SURE CONTRACT WITHIN START/END            
         BL    GET4D               GO CHK END                                   
         CLC   PCONSDT,BQEND       SEE IF STARTS PAST REQ END                   
         BH    GET4                                                             
         B     GET4X                                                            
*                                                                               
GET4D    CLC   PCONEDT,BQSTART     SEE IF ENDS BEFORE REQ START                 
         BL    GET4                                                             
*                                                                               
GET4X    MVC   SAVKEY,KEY                                                       
         B     GET7                  SKIP READING FOR BUYS                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),SAVKEY                                                   
         MVI   KEY+3,X'21'                                                      
         BAS   RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GET7                                                             
         MVC   KEY,SAVKEY                                                       
         BAS   RE,HIGH                                                          
         B     GET4                                                             
GET7     DS    0H                                                               
*                                                                               
         MVC   KEY,SAVKEY                                                       
         BAS   RE,READ                                                          
         OI    KEY+25,X'80'                                                     
         BAS   RE,WRITE                                                         
         MVC   P(3),KEY+4                                                       
         GOTO1 PUBEDIT,DMCB,KEY+7,P+6                                           
         MVC   HALF,KEY+13                                                      
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+23(4),DUB                                                      
         MVC   P+30(3),PCONPRD                                                  
         GOTO1 DATCON,DMCB,(3,PCONSDT),(5,P+35)                                 
         GOTO1 (RF),(R1),(3,PCONEDT),(5,P+44)                                   
         MVI   P+43,C'-'                                                        
         BAS   RE,RPRT                                                          
         AP    PRGCON,=P'1'                                                     
GET8     DS    0H                                                               
         MVC   KEY,SAVKEY                                                       
         CLI   RCWRITE,C'Y'                                                     
         BE    GET2                                                             
         BAS   RE,HIGH                                                          
         B     GET4                                                             
*                                                                               
END      DS    0H                                                               
         BAS   RE,RPRT                                                          
         EDIT  (P5,PRGCON),(9,P),COMMAS=YES                                     
         MVC   P+10(16),=C'CONTRACTS PURGED'                                    
         BAS   RE,RPRT                                                          
         B     EXIT                                                             
PRGCON   DC    PL5'0'                                                           
SAVKEY   DS    XL64                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVC   HEAD3(15),=C'**FILE MARKED**'                                    
         CLI   RCWRITE,C'Y'                                                     
         BE    *+10                                                             
         MVC   HEAD3(19),=C'**FILE NOT MARKED**'                                
*                                                                               
         GOTO1 DATCON,DMCB,QSTART,(5,HEAD3+51)                                  
         MVC   HEAD3+60(4),=C'THRU'                                             
         GOTO1 DATCON,DMCB,QEND,(5,HEAD3+65)                                    
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
         BAS   RE,HIPUB                                                         
         B     *+8                                                              
NP2      DS    0H                                                               
         BAS   RE,SEQPUB                                                        
         MVI   PUBKMED,X'FF'       SET EOF                                      
         CLC   KEY(1),KEYSAVE                                                   
         BNE   NPX                                                              
         CLC   KEY+7(2),KEYSAVE+7                                               
         BNE   NP2                                                              
         CLI   KEY+9,X'81'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LASTPUB,KEY+1                                                    
         BAS   RE,GETPUB                                                        
         XC    LTLREC(50),LTLREC                                                
         BAS   RE,SEQPUB                                                        
         CLC   KEY(9),PUBKEY                                                    
         BNE   NPX                                                              
         BAS   RE,GETLTL                                                        
*                                                                               
NPX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
RDPUB    DS    0H                                                               
         LA    RF,DMREAD                                                        
         B     DIRPUB                                                           
WRTPUB   DS    0H                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNER  RE                                                               
         LA    RF,DMWRT                                                         
         B     DIRPUB                                                           
HIPUB    DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         LA    RF,DMRDHI                                                        
         B     DIRPUB                                                           
SEQPUB   DS    0H                                                               
         LA    RF,DMRSEQ                                                        
*                                                                               
DIRPUB   DS    0H                                                               
         ST    RF,DMCB                                                          
         LR    R0,RE                                                            
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,,PUBDIR,KEY,KEY                                     
*                                                                               
         B     DMCHK                                                            
*                                                                               
PUTPUB   DS    0H                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNER  RE                                                               
         LA    RF,PUTREC                                                        
         B     *+8                                                              
GETPUB   DS    0H                                                               
         LA    RF,GETREC                                                        
         ST    RF,DMCB                                                          
         LA    RF,PUBREC                                                        
         ST    RF,DMCB+12                                                       
         LA    RF,PUBDMWRK                                                      
         ST    RF,DMCB+16                                                       
         B     FILPUB                                                           
*                                                                               
PUTLTL   DS    0H                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNER  RE                                                               
         LA    RF,PUTREC                                                        
         B     *+8                                                              
GETLTL   DS    0H                                                               
         LA    RF,GETREC                                                        
         ST    RF,DMCB                                                          
         LA    RF,LTLREC                                                        
         ST    RF,DMCB+12                                                       
         LA    RF,LTLDMWRK                                                      
         ST    RF,DMCB+16                                                       
*                                                                               
FILPUB   DS    0H                                                               
         LR    R0,RE                                                            
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,,PUBFILE,KEY+27                                     
*                                                                               
         B     DMCHK                                                            
*                                                                               
*                                                                               
DMCHK    DS    0H                                                               
         LR    RE,R0                                                            
         MVC   BYTE,DMOUTBTS                                                    
         NC    BYTE,DMCB+8                                                      
         BZR   RE                                                               
         DC    H'0'                                                             
         EJECT                                                                  
HIGH     DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         LA    RF,DMRDHI                                                        
         B     DIRPRT                                                           
SEQ      DS    0H                                                               
         LA    RF,DMRSEQ                                                        
         B     DIRPRT                                                           
READ     DS    0H                                                               
         LA    RF,DMREAD                                                        
         B     DIRPRT                                                           
WRITE    DS    0H                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNER  RE                                                               
         LA    RF,DMWRT                                                         
*                                                                               
DIRPRT   DS    0H                                                               
         ST    RF,DMCB                                                          
         LR    R0,RE                                                            
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,,PRTDIR,KEY,KEY                                     
*                                                                               
         B     DMCHK                                                            
*                                                                               
*                                                                               
GETPRT   DS    0H                                                               
         LA    RF,GETREC                                                        
         B     FILPRT                                                           
*                                                                               
PUTPRT   DS    0H                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNER  RE                                                               
         LA    RF,PUTREC                                                        
*                                                                               
FILPRT   DS    0H                                                               
         ST    RF,DMCB                                                          
         LR    R0,RE                                                            
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,,PRTFILE,KEY+27,AREC,DMWORK                         
*                                                                               
         B     DMCHK                                                            
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
         GOTO1 =V(HEXOUT),DMCB,(R5),P+01,(R2),=C'N',RR=RELO                     
*                                                                               
         MVC   WORK(25),0(R5)                                                   
         TR    WORK(25),TRTAB                                                   
         MVC   P+75(25),WORK                                                    
         B     EXIT                                                             
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         L     R5,AREC                                                          
         MVC   HALF,25(R5)                                                      
         LH    R2,HALF                                                          
         LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 =V(HEXOUT),DMCB,(R5),WORK,(R4),=C'N',RR=RELO                     
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
PDUMREC  DS    CL1000                                                           
         EJECT                                                                  
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
AREC     DS    A                                                                
RELO     DS    A                                                                
ELCODE   DS    X                                                                
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
*                                                                               
*        PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPWORKD                                                        
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019PPREPCP02 05/01/02'                                      
         END                                                                    
