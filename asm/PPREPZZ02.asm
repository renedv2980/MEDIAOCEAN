*          DATA SET PPREPZZ02  AT LEVEL 007 AS OF 05/01/02                      
*PHASE PPZZ02A,+0                                                               
*INCLUDE IJFVZZWZ                                                               
         TITLE 'PPZZ02 - PRINT ZZ PUB UPDATE'                                   
PPZZ02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPZZ02                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     R9,PPFILEC                                                       
         USING PPFILED,R9                                                       
         LA    RC,SPACEND                                                       
         USING PPZZWRKD,RC                                                      
         SPACE 2                                                                
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
PROC     DS    0H                                                               
         LA    R1,RECOV                                                         
         OPENR (1)                                                              
*                                                                               
         ZAP   ERRCNT,=P'0'                                                     
         ZAP   ADDCNT,=P'0'                                                     
         ZAP   CHGCNT,=P'0'                                                     
*                                                                               
PR4      DS    0H                                                               
         MVI   ERROR,0                                                          
         LA    R1,RECOV                                                         
         LA    R0,RECVHDR-4                                                     
         GET   (1),(0)                                                          
*                                                                               
**NEW  1/30/90             WAS X'13'                                            
         CLI   RFILTY,X'43'        PUB FILE ONLY                                
         BNE   PR4                                                              
*                                                                               
         CLC   RKEY(1),QMEDIA      TEST MEDIA                                   
         BNE   PR4                                                              
         CLC   RKEY+7(2),=C'ZZ'    TEST STANDARD FILE RECORD                    
         BNE   PR4                                                              
*                                                                               
         CLI   RRECTY,2            CHANGE                                       
         BE    PR10                                                             
         CLI   RRECTY,3            ADD                                          
         BE    PR20                                                             
         B     PR4                                                              
PR10     DS    0H                                                               
         MVC   KEY(25),RKEY                                                     
         BAS   RE,HIPUB                                                         
         CLC   KEY(25),KEYSAVE                                                  
         BE    PR12                                                             
         MVI   ERROR,CHGERR                                                     
         B     PR40                                                             
*                                                                               
PR12     DS    0H                                                               
         AP    CHGCNT,=P'1'                                                     
         BAS   RE,GETPUB                                                        
         BAS   RE,PUTPUB                                                        
         B     PR40                                                             
         SPACE 3                                                                
PR20     DS    0H                                                               
         MVC   KEY(25),RKEY                                                     
         BAS   RE,HIPUB                                                         
         CLC   KEY(25),KEYSAVE                                                  
         BNE   PR22                                                             
         MVI   ERROR,ADDERR                                                     
         B     PR40                                                             
*                                                                               
PR22     DS    0H                                                               
         AP    ADDCNT,=P'1'                                                     
         BAS   RE,ADDPUB                                                        
         B     PR40                                                             
         SPACE 3                                                                
PR40     DS    0H                                                               
         MVC   P+2(1),RKEY                                                      
         GOTO1 PUBEDIT,DMCB,RKEY+1,(C'S',P+8)                                   
         LA    R3,RKEY+PUBNAME-PUBREC                                           
         MVC   P+27(20),0(R3)                                                   
         MVC   P+48(20),20(R3)                                                  
*                                                                               
         MVC   P+70(3),=C'ADD'                                                  
         CLI   RRECTY,3                                                         
         BE    *+10                                                             
         MVC   P+70(3),=C'CHG'                                                  
         CLI   ERROR,0                                                          
         BE    PR44                                                             
*                                                                               
         AP    ERRCNT,=P'1'                                                     
         MVC   P+70(29),=C'**CHANGE OF REC NOT ON FILE**'                       
         CLI   ERROR,CHGERR                                                     
         BE    *+10                                                             
         MVC   P+70(30),=C'*-ADD OF REC ALREADY ON FILE**'                      
*                                                                               
PR44     DS    0H                                                               
         BAS   RE,RPRT                                                          
         B     PR4                                                              
         SPACE 3                                                                
RPRT     NTR1                                                                   
         MVC   HEAD3(13),=C'SYSTEM = PRNT'                                      
         MVC   HEAD3+14(1),FILENUM                                              
         MVC   HEAD5(25),=C'PRNT1 RECOVERY TAPE DATED'                          
         MVC   HEAD5+26(8),TDATE                                                
         GOTO1 REPORT                                                           
         XIT1                                                                   
         SPACE 3                                                                
HIPUB    NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,PUBDIR,KEY,KEY                               
         B     DMCHK                                                            
         SPACE 3                                                                
GETPUB   DS    0H                                                               
         LA    RF,PUBREC                                                        
         ST    RF,DMCB+12                                                       
         LA    RF,GETREC                                                        
         ST    RF,DMCB                                                          
         LA    RF,KEY+27                                                        
         ST    RF,DMCB+8                                                        
         B     FILPUB                                                           
         SPACE 2                                                                
ADDPUB   DS    0H                                                               
         LA    RF,RKEY                                                          
         ST    RF,DMCB+12                                                       
         LA    RF,ADDREC                                                        
         ST    RF,DMCB                                                          
         LA    RF,KEY                                                           
         ST    RF,DMCB+8                                                        
         B     FILPUB                                                           
         SPACE 2                                                                
PUTPUB   DS    0H                                                               
         LA    RF,RKEY                                                          
         ST    RF,DMCB+12                                                       
         LA    RF,PUTREC                                                        
         ST    RF,DMCB                                                          
         LA    RF,KEY+27                                                        
         ST    RF,DMCB+8                                                        
         SPACE 2                                                                
FILPUB   NTR1                                                                   
         GOTO1 DATAMGR,DMCB,,PUBFILE,,,DMWORK                                   
         SPACE 2                                                                
DMCHK    DS    0H                                                               
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
RECVEOT  DS    0H                                                               
         LA    R1,RECOV                                                         
         CLOSER (1)                                                             
*                                                                               
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         MVC   P(04),=C'ADDS'                                                   
         EDIT  (P5,ADDCNT),(5,P+10)                                             
         BAS   RE,RPRT                                                          
         MVC   P(07),=C'CHANGES'                                                
         EDIT  (P5,CHGCNT),(5,P+10)                                             
         BAS   RE,RPRT                                                          
         MVC   P(06),=C'ERRORS'                                                 
         EDIT  (P5,ERRCNT),(5,P+10)                                             
         BAS   RE,RPRT                                                          
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
RECVLBL  DS    0H                                                               
         CH    R0,HCODE                                                         
         BNE   LBL2                                                             
         MVC   P(80),0(R1)                                                      
         BAS   RE,RPRT                                                          
         MVC   TDATE,0(R1)                                                      
*                                                                               
LBL2     DS    0H                                                               
         LBRET 2                                                                
*                                                                               
         DS    0H                                                               
HCODE    DC    X'00'                                                            
         DC    C'O'                                                             
         SPACE 3                                                                
RECOV    DTFMT DEVADDR=SYS008,BLKSIZE=8500,RECFORM=VARBLK,             X        
               TYPEFLE=INPUT,IOAREA1=RECOV1,WORKA=YES,FILABL=STD,      X        
               EOFADDR=RECVEOT,LABADDR=RECVLBL                                  
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
RECOV1   DS    8500C                                                            
         EJECT                                                                  
PPZZWRKD DSECT                                                                  
ERROR    DS    X                                                                
ADDCNT   DS    PL5                                                              
CHGCNT   DS    PL5                                                              
ERRCNT   DS    PL5                                                              
TDATE    DS    CL8                                                              
CHGERR    EQU   1                                                               
ADDERR   EQU   2                                                                
         SPACE 2                                                                
*                                                                               
       ++INCLUDE DMRCVRHDR                                                      
*                                                                               
RKEY     DS    CL25                                                             
         DS    2000C                                                            
         SPACE 2                                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPWORKD                                                        
       ++INCLUDE PPNEWFILE                                                      
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007PPREPZZ02 05/01/02'                                      
         END                                                                    
