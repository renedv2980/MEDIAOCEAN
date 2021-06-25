*          DATA SET PPREP02202 AT LEVEL 010 AS OF 05/01/02                      
*PHASE PP0202A,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0202 - VERIFY 20 21 DIRECTORY PTRS'                           
         PRINT NOGEN                                                            
PP0202   CSECT                                                                  
         NMOD1 0,PP0202                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
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
         ZAP   INCNT,=P'0'                                                      
         ZAP   TAXCNT,=P'0'                                                     
         B     EXIT                                                             
*                                                                               
RECODE   DC    X'20'                                                            
PROC     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
*        CLI   QOPT5,C'N'          MEANS DON'T MARK FILE                        
*        BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
*                                                                               
AGYC1A   XC    KEY,KEY                                                          
         MVC   KEY(3),QOPT1                                                     
         MVC   KEY+3(1),RECODE     BUYS                                         
AGYC2    GOTO1 HIGH                                                             
         AP    COUNS,=P'1'                                                      
         B     AGYC4                                                            
*                                                                               
AGYC3    DS    0H                                                               
         GOTO1 SEQ                                                              
         AP    COUNT,=P'1'                                                      
AGYC4    DS    0H                                                               
         CP    COUNT,=P'200000'                                                 
         BH    *-2                                                              
         CLC   KEY(2),QOPT1       ONLY MATCH AGENCY/MEDIA                       
         BE    AGCY5              DONE                                          
         CLI   RECODE,X'21'                                                     
         BE    EXIT                                                             
         MVI   RECODE,X'21'                                                     
         B     AGYC1A                                                           
*                                                                               
*                                                                               
AGCY5    CLC   KEY+3(1),RECODE       SEE IF BUY RECORD                          
         BNE   AGYC3                                                            
*                                                                               
*                                                                               
AGYC6    DS    0H AT THIS POINT MUST READ THE MIRROR OF 21 OR 22                
         MVC   SAVEK,KEY                                                        
         CLI   SAVEK+3,X'20'                                                    
         BNE   MB21PTR                                                          
         CLC   SAVEK+21(3),=C'ZZZ'                                              
         BE    AGYC3               POOL HAS NO 21 PTRS                          
         MVI   KEY+3,X'21'                                                      
         MVC   KEY+7(6),KEY+10                                                  
         MVC   KEY+13(3),SAVEK+7                                                
RDDHI    GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   MISSKEY                                                          
RETRN    DS    0H                                                               
         MVC   KEY,SAVEK                                                        
         GOTO1 HIGH                                                             
         B     AGYC3                  READ SEQ                                  
*                                                                               
MB21PTR  MVI   KEY+3,X'20'                                                      
         MVC   KEY+7(3),SAVEK+13                                                
         MVC   KEY+10(6),SAVEK+7                                                
         B     RDDHI                                                            
MISSKEY  DS     0H                                                              
         GOTO1 HEXOUT,DMCB,KEYSAVE,P,33,=C'N'                                   
         MVC   P+70(33),KEYSAVE                                                 
         BAS   RE,RPRT                                                          
         AP    TAXCNT,=P'1'                                                     
*                                                                               
         CP    TAXCNT,=P'400'                                                   
         BH    *-2                                                              
         B     RETRN                                                            
*                                                                               
SAVEK    DC    XL33'0'                                                          
*                                                                               
COUNTS   DS    0C                                                               
INCNT    DS    PL8                                                              
         DC    CL15'BUYRECS READ'                                               
TAXCNT   DS    PL8                                                              
         DC    CL15'RECORDS DUMPED '                                            
COUNT    DC    PL8'0'                                                           
         DC    CL15'DIRECTORY SEQ  '                                            
COUNS    DC    PL8'0'                                                           
         DC    CL15'DIRECTORY HIGH '                                            
         DC    X'FF'                                                            
*                                                                               
CNT24    DS    PL4                                                              
CNT70    DS    PL4                                                              
OLDTAX   DS    CL3                                                              
*                                                                               
OUTSPC   DS    CL40                                                             
************************************* OLD CODE                                  
         LA    R7,PBUYREC                                                       
         ST    R7,AREC                                                          
         GOTO1 GETPRT                                                           
         GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKPRD                               
         OC    PGROSS,PGROSS                                                    
         BZ    PROC3                                                            
         TM    PBUYREC+27,X'80'       SEE IF DELETED AND PAID                   
         BZ    PROC3                                                            
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,P,33,=C'N'                                       
         BAS   RE,RPRT                                                          
         B     PROC3                                                            
************************ END OF PROGRAM                                         
         CLC   KEY(4),KEYSAVE                                                   
         BNE   EXIT                                                             
*                                                                               
         MVC   WORK(32),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(1),WORK+2      MEDIA                                         
         MVC   KEY+1(6),WORK+10      PUB                                        
         MVC   KEY+7(2),WORK         AGY                                        
         MVI   KEY+9,X'81'                                                      
         GOTO1 HIGHPUB                                                          
         CLC   KEY(10),KEYSAVE                                                  
         BE    PROC8                                                            
         GOTO1 HEXOUT,DMCB,KEYSAVE,P,33,=C'N'                                   
         BAS   RE,RPRT                                                          
*                                                                               
PROC8    MVC   KEY,WORK                                                         
         GOTO1 HIGH                                                             
         B     PROC3                                                            
*                                                                               
         LA    R7,PBUYREC                                                       
         ST    R7,AREC                                                          
         GOTO1 GETPRT                                                           
         CLC   KEY(15),0(R7)                                                    
         BE    PROC3                                                            
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,P,33,=C'N'                                       
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,0(R7),P,65,=C'N'                                     
         BAS   RE,RPRT                                                          
         B     PROC3                                                            
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
         DS    CL4000                                                           
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
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
**PAN#1  DC    CL21'010PPREP0220205/01/02'                                      
         END                                                                    
