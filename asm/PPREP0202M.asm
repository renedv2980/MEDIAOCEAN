*          DATA SET PPREP0202M AT LEVEL 042 AS OF 05/01/02                      
*PHASE PP0202M,+0,NOAUTO                                                        
*INCLUDE PPBVAL                                                                 
*INCLUDE MININAM                                                                
         TITLE 'PP0202 - PRTFIX PROGRAM'                                        
*                                                                               
*        THIS PROGRAM COPYS CONTRACTS, REPS, AND STANDARD COMMENTS              
*        FROM ONE AGY TO ANOTHER                                                
*                                                                               
*******  IT ALSO CHECKS FOR PRODUCT CONTRACTS AND PRODUCT RATES                 
*******  AND WILL NOT COPY THEM.                                                
*                                                                               
*        FROM AGY/MED/CLT IS IN QAGY/QMEDIA/QCLIENT                             
*        TO AGY/MED/CLT   IS IN QPAY(6) COL 53                                  
*                                                                               
*        NOTE - CLT ONLY NEEDED FOR CONTRACTS                                   
*                                                                               
*        QOPT1 C= COPY CONTRACTS                                                
*        QOPT2 R= REPS                                                          
*        QOPT3 S= STANDARD COMMENTS                                             
*                                                                               
*                                                                               
         PRINT NOGEN                                                            
PP0202   CSECT                                                                  
         NMOD1 0,PP0202                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         MVI   RC2DSECT,C'Y'                                                    
         L     R7,PPWORK2C                                                      
         USING PPWORK2D,R7                                                      
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
**                                                                              
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   CONCNT,=P'0'    CONTRACTS READ                                   
         ZAP   REPCNT,=P'0'    REPS READ                                        
         ZAP   STDCNT,=P'0'    STD COMMS READ                                   
         ZAP   CONCPY,=P'0'    CONTRACTS COPIED                                 
         ZAP   REPCPY,=P'0'    REPS COPIED                                      
         ZAP   STDCPY,=P'0'    STD COMMS COPIED                                 
         ZAP   CONCNTR,=P'0'   CONTRACTS READ                                   
         ZAP   REPCNTR,=P'0'   REPS READ                                        
         ZAP   STDCNTR,=P'0'   STD COMMS READ                                   
         ZAP   CONCPYR,=P'0'   CONTRACTS COPIED                                 
         ZAP   REPCPYR,=P'0'   REPS COPIED                                      
         ZAP   STDCPYR,=P'0'   STD COMMS COPIED                                 
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   QOPT1,C'C'           SEE IF COPYING CONTRACTS                    
         BNE   REP                                                              
         MVC   P1+5(16),=C'CONTRACTS COPIED'                                    
         BAS   RE,RPRT                                                          
         MVC   P1+5(16),=C'----------------'                                    
         BAS   RE,RPRT                                                          
         BAS   RE,RPRT                                                          
*                                                                               
         LA    R0,PCONREC                                                       
         ST    R0,AREC                                                          
*                                                                               
CON      XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      START AT AGENCY                              
         MVI   KEY+3,X'10'         CONTRACTS                                    
         MVC   KEY+4(3),QCLIENT                                                 
*                                                                               
CON2     GOTO1 HIGH                                                             
         B     CON4                                                             
*                                                                               
CON3     DS    0H                                                               
         GOTO1 SEQ                                                              
CON4     DS    0H                                                               
         CLI   KEY,X'FF'           END OF FILE                                  
         BE    REP                                                              
         CLC   KEY(7),KEYSAVE      AGY/MED/CLT                                  
         BNE   REP                 END OF CONTRACTS                             
*                                                                               
CON6     DS    0H                                                               
         AP    CONCNT,=P'1'                                                     
*                                                                               
         GOTO1 GETPRT                                                           
         CLI   PCONPRD,C'A'         CHECK FOR PRODUCT CONTRACT                  
         BL    CON6C                                                            
         MVC   P1+1(25),KEY                                                     
         MVC   P1+30(3),PCONPRD                                                 
         MVC   P1+40(22),=C'** PRODUCT CONTRACT **'                             
         BAS   RE,RPRT                                                          
         B     CON3                                                             
*                                                                               
CON6C    DS    0H                                                               
         CLI   QMEDIA,C'N'                                                      
         BE    CON6K                                                            
         MVI   ELCODE,X'20'                                                     
CON6C2   LA    R2,PCONREC+33                                                    
CON6C5   BAS   RE,NEXTEL                                                        
         BNE   CON6E                                                            
         USING PRBELEM,R2                                                       
         CLI   PRBOPEN,C'A'     MIGHT CONTAIN PRD IF NON-NEWS                   
         BL    CON6C5                                                           
         MVC   P1+1(25),KEY                                                     
         MVC   P1+30(5),PRBOPEN                                                 
         MVC   P1+40(24),=C'** PRODUCT RATE FOUND **'                           
         BAS   RE,RPRT                                                          
         B     CON3                                                             
*                                                                               
         DROP  R2                                                               
*                                                                               
CON6E    CLI   ELCODE,X'22'        SEE IF I JUST DID LAST ELCODE                
         BE    CON6K               IF YES, THEN DONE                            
         CLI   ELCODE,X'21'        SEE IF I JUST DID X'21'                      
         BNE   CON6E5                                                           
         MVI   ELCODE,X'22'        IF SO, NOW DO X'22'                          
         B     CON6C2                                                           
*                                                                               
CON6E5   MVI   ELCODE,X'21'        I MUST HAVE JUST DONE X'20'                  
         B     CON6C2              SO NOW DO X'21'                              
*                                                                               
CON6K    DS    0H                                                               
*                                                                               
CON6X    DS    0H                                                               
*                                                                               
         AP    CONCPY,=P'1'                                                     
*                                                                               
COPYCON  DS    0H                                                               
         MVC   OLDKEY,KEY                                                       
         XC    KEY,KEY                                                          
         MVC   PCONKAGY(3),QPAY     NEW AGY/MED                                 
         MVC   PCONKCLT,QPAY+3      NEW CLT                                     
         MVC   KEY(25),PCONREC                                                  
         GOTO1 ADDPRT                                                           
         MVC   P1+3(4),=C'OLD='                                                 
         MVC   P1+10(25),OLDKEY                                                 
         MVC   P1+52(4),=C'NEW='                                                
         MVC   P1+60(25),KEY                                                    
         MVC   KEY,OLDKEY                                                       
         BAS   RE,RPRT                                                          
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         GOTO1 HIGH                                                             
         B     CON3                                                             
*                                                                               
         EJECT                                                                  
REP      DS    0H                   REPS                                        
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   QOPT2,C'R'           SEE IF COPYING REPS                         
         BNE   STD                                                              
*                                                                               
         MVC   P1+5(11),=C'REPS COPIED'                                         
         BAS   RE,RPRT                                                          
         MVC   P1+5(11),=C'-----------'                                         
         BAS   RE,RPRT                                                          
         BAS   RE,RPRT                                                          
*                                                                               
         LA    R0,PREPREC                                                       
         ST    R0,AREC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      START AT AGENCY                              
         MVI   KEY+3,X'11'         REP                                          
*                                                                               
REP2     GOTO1 HIGH                                                             
         B     REP4                                                             
*                                                                               
REP3     DS    0H                                                               
         GOTO1 SEQ                                                              
REP4     DS    0H                                                               
         CLI   KEY,X'FF'           END OF FILE                                  
         BE    STD                                                              
         CLC   KEY(4),KEYSAVE      AGY/MED                                      
         BNE   STD                 END OF REPS                                  
*                                                                               
REP6     DS    0H                                                               
         GOTO1 GETPRT                                                           
         AP    REPCNT,=P'1'                                                     
*                                                                               
REP6X    DS    0H                                                               
*                                                                               
         AP    REPCPY,=P'1'                                                     
*                                                                               
COPYREP  DS    0H                                                               
         MVC   OLDKEY,KEY                                                       
         XC    KEY,KEY                                                          
         MVC   PREPKAGY(3),QPAY     NEW AGY/MED                                 
         MVC   KEY(25),PREPREC                                                  
         GOTO1 ADDPRT                                                           
         MVC   P1+3(4),=C'OLD='                                                 
         MVC   P1+10(32),OLDKEY                                                 
         MVC   P1+52(4),=C'NEW='                                                
         MVC   P1+60(32),KEY                                                    
         MVC   KEY,OLDKEY                                                       
         BAS   RE,RPRT                                                          
         GOTO1 HIGH                                                             
         B     REP3                                                             
*                                                                               
         EJECT                                                                  
STD      DS    0H                   STANDARD COMMENTS                           
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   QOPT3,C'S'           SEE IF COPYING STDS                         
         BNE   ENDREQ                                                           
*                                                                               
         MVC   P1+5(24),=C'STANDARD COMMENTS COPIED'                            
         BAS   RE,RPRT                                                          
         MVC   P1+5(11),=C'------------------------'                            
         BAS   RE,RPRT                                                          
         BAS   RE,RPRT                                                          
*                                                                               
         LA    R0,PBUYREC         READ INTO PBUYREC                             
         ST    R0,AREC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      START AT AGENCY                              
         MVI   KEY+3,X'40'         STD                                          
*                                                                               
STD2     GOTO1 HIGH                                                             
         B     STD4                                                             
*                                                                               
STD3     DS    0H                                                               
         GOTO1 SEQ                                                              
STD4     DS    0H                                                               
         CLI   KEY,X'FF'           END OF FILE                                  
         BE    ENDREQ                                                           
         CLC   KEY(4),KEYSAVE      AGY/MED                                      
         BNE   ENDREQ              END OF STDS                                  
*                                                                               
STD6     DS    0H                                                               
         GOTO1 GETPRT                                                           
         AP    STDCNT,=P'1'                                                     
*                                                                               
STD6X    DS    0H                                                               
*                                                                               
         AP    STDCPY,=P'1'                                                     
*                                                                               
COPYSTD  DS    0H                                                               
         MVC   OLDKEY,KEY                                                       
         XC    KEY,KEY                                                          
         MVC   PBUYREC(3),QPAY     NEW AGY/MED                                  
         MVC   KEY(25),PBUYREC                                                  
         GOTO1 ADDPRT                                                           
         MVC   P1+3(4),=C'OLD='                                                 
         MVC   P1+10(32),OLDKEY                                                 
         MVC   P1+52(4),=C'NEW='                                                
         MVC   P1+60(32),KEY                                                    
         MVC   KEY,OLDKEY                                                       
         BAS   RE,RPRT                                                          
         GOTO1 HIGH                                                             
         B     STD3                                                             
*                                                                               
ENDREQ   DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   P1(12),=C'MEDIA TOTALS'                                          
         MVC   P2(12),=C'------------'                                          
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         LA    R4,RCOUNTS                                                       
         LA    R5,COUNTS                                                        
ENDR5    CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         MVC   P1(15),8(R4)                                                     
*                                                                               
         AP    0(8,R5),0(8,R4)   ROLL TO RUN TOTALS                             
*                                                                               
         OI    7(R4),X'0F'                                                      
         UNPK  P1+20(10),0(8,R4)                                                
         GOTO1 REPORT                                                           
         MVC   0(8,R4),=PL8'0'                                                  
         LA    R4,23(R4)         NEXT RCOUNT                                    
         LA    R5,23(R5)         NEXT COUNT                                     
         B     ENDR5                                                            
*                                                                               
*                                                                               
*                                                                               
RCOUNTS  DS    0C                REQUEST TOTALS                                 
CONCNT   DS    PL8                                                              
         DC    CL15'CONTRACTS READ'                                             
REPCNT   DS    PL8                                                              
         DC    CL15'REPS READ'                                                  
STDCNT   DS    PL8                                                              
         DC    CL15'STD COMMS READ'                                             
CONCPY   DS    PL8                                                              
         DC    CL15'CONS. COPIED'                                               
REPCPY   DS    PL8                                                              
         DC    CL15'REPS COPIED'                                                
STDCPY   DS    PL8                                                              
         DC    CL15'STD COMS COPIED'                                            
         DC    X'FF'                                                            
*                                                                               
COUNTS   DS    0C                RUN TOTALS                                     
CONCNTR  DS    PL8                                                              
         DC    CL15'CONTRACTS READ'                                             
REPCNTR  DS    PL8                                                              
         DC    CL15'REPS READ'                                                  
STDCNTR  DS    PL8                                                              
         DC    CL15'STD COMMS READ'                                             
CONCPYR  DS    PL8                                                              
         DC    CL15'CONS. COPIED'                                               
REPCPYR  DS    PL8                                                              
         DC    CL15'REPS COPIED'                                                
STDCPYR  DS    PL8                                                              
         DC    CL15'STD COMS COPIED'                                            
         DC    X'FF'                                                            
*                                                                               
SAVKEY   DS    CL32                                                             
OLDKEY   DS    CL32                                                             
CNT24    DS    PL4                                                              
CNT70    DS    PL4                                                              
OLDDATE  DS    CL3                                                              
ERRSW    DS    CL1                                                              
DDSBLD   DS    CL1                                                              
DATETYP  DS    CL1                                                              
KEYPSW   DS    CL1                                                              
*                                                                               
OUTSPC   DS    CL40                                                             
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   P1(10),=C'RUN TOTALS'                                            
         MVC   P2(10),=C'----------'                                            
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         MVC   P1(15),8(R4)                                                     
         OI    7(R4),X'0F'                                                      
         UNPK  P1+20(10),0(8,R4)                                                
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
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
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
         GOTO1 HEXOUT,DMCB,(R5),P1+01,(R2),=C'N'                                
*                                                                               
         MVC   WORK(25),0(R5)                                                   
         TR    WORK(25),TRTAB                                                   
         MVC   P1+75(25),WORK                                                   
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
         MVC   P1+01(8),WORK+00                                                 
         MVC   P1+10(8),WORK+08                                                 
         MVC   P1+19(8),WORK+16                                                 
         MVC   P1+28(8),WORK+24                                                 
         MVC   P1+37(8),WORK+32                                                 
         MVC   P1+46(8),WORK+40                                                 
         MVC   P1+55(8),WORK+48                                                 
         MVC   P1+64(8),WORK+56                                                 
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P1+75(0),WORK                                                    
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
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
FRSTSW   DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
PPBVWORK DS    0D                                                               
       ++INCLUDE PPBVALD                                                        
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042PPREP0202M05/01/02'                                      
         END                                                                    
