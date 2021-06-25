*          DATA SET PPREP9602  AT LEVEL 055 AS OF 05/01/02                      
*PHASE PP9602A,+0,NOAUTO                                                        
         TITLE 'PP9602 - LIST PROGRAM'                                          
*                                                                               
         PRINT NOGEN                                                            
PP9602   CSECT                                                                  
         NMOD1 0,PP9602                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP96WRKD,R8                                                      
         LA    R0,PBILLREC                                                      
         ST    R0,AREC                                                          
**                                                                              
*        ZIC   R2,MODE                                                          
*        MVC   P(7),=C'MODE = '                                                 
*        EDIT  (R2),(4,P+9)                                                     
*        GOTO1 REPORT                                                           
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    EXIT                                                             
         CLI   MODE,X'5A'          90, FIRST FOR EVERY REQUEST                  
         BE    PROC                                                             
         CLI   MODE,RUNLAST        AFTER ALL REQUESTS                           
         BE    EXIT                                                             
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   H1+51(29),=C'PRINTPAK P&&G ESTIMATE LISTING'                     
         MVC   H3+9(3),QCLIENT                                                  
         CLC   QPRODUCT,=C'   '                                                 
         BE    PROC1                                                            
         MVC   H5(8),=C'PRODUCT '                                               
         MVC   H5+9(3),QPRODUCT                                                 
*                                                                               
PROC1    XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      AGENCY AND MEDIA                             
         MVI   KEY+3,X'0A'         CLIENTS                                      
         MVC   KEY+4(3),QCLIENT    ONE CLIENT                                   
         CLC   QPRODUCT,=C'   '    IS THERE A PRODUCT?                          
         BE    AGYC2               NO, USE ALL PRODUCT                          
         MVC   KEY+7(3),QPRODUCT   YES, PUT PRODUCT IN KEY                      
*                                                                               
AGYC2    GOTO1 HIGH                                                             
         B     AGYC4                                                            
*                                                                               
AGYC3    DS    0H                                                               
         GOTO1 SEQ                                                              
AGYC4    DS    0H                                                               
         CLI   KEY+3,X'0A'                                                      
         BNE   EXIT                                                             
         CLC   KEY(3),QAGENCY     ONLY MATCH AGENCY/MEDIA                       
         BNE   EXIT               NO, DONE                                      
         CLC   KEY+4(3),QCLIENT   MATCH CLIENT?                                 
         BNE   EXIT               NO, FINISHED.                                 
         CLC   QPRODUCT,=C'   '   USING ALL PRODUCTS?                           
         BE    AGYC6              YES, CONTINUE.                                
         CLC   KEY+7(3),QPRODUCT  ONLY MATCH PRODUCT                            
         BNE   EXIT               NO, FINISHED.                                 
*                                                                               
         PRINT GEN                                                              
AGYC6    LA    R0,PBILLREC                                                      
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         PRINT NOGEN                                                            
         LA    R3,P                                                             
         MVC   H10(80),=C'ESTIMATE  CHRG PER  ACCOUNT   PG BRAND  PG ESX        
               T    EVENT CD  MLT-BRND  NO BRAND  '                             
         MVC   H11(80),=C'--------  --------  -------   --------  -----X        
               -    --------  --------- --------  '                             
         CLC   QPRODUCT,=C'   '                                                 
         BNE   AGYC6A                                                           
         MVC   H10(90),=C'PRODUCT   ESTIMATE  CHRG PER  ACCOUNT   PG BRX        
               AND  PG EST    EVENT CD  MLT-BRND  NO BRAND  '                   
         MVC   H11(90),=C'-------   --------  --------  -------   -----X        
               ---  ------    --------  --------  ----------'                   
         MVC   2(3,R3),KEY+7                                                    
         LA    R3,10(R3)                                                        
AGYC6A   LA    R4,PBILLREC                                                      
         SR    R2,R2                                                            
         LH    R2,10(R4)                                                        
         EDIT  (R2),(5,1(R3))                                                   
         LA    R3,10(R3)                                                        
         LA    R4,33(R4)           SKIP PAST THE KEY                            
*                                                                               
         MVC   2(3,R3),11(R4)      JUST USE DATA, CHARGE PERIOD                 
         LA    R3,10(R3)                                                        
         LA    R4,35(R4)           NEXT ONE                                     
*                                                                               
         MVC   2(3,R3),11(R4)      JUST USE DATA, ACCOUNT                       
         LA    R3,10(R3)                                                        
         LA    R4,35(R4)           NEXT ONE                                     
*                                                                               
         MVC   2(3,R3),11(R4)      JUST USE DATA, PG BRAND                      
         LA    R3,10(R3)                                                        
         LA    R4,35(R4)           NEXT ONE                                     
*                                                                               
         MVC   0(4,R3),11(R4)      JUST USE DATA, PG EST                        
         LA    R3,10(R3)                                                        
         LA    R4,35(R4)           NEXT ONE                                     
*                                                                               
         MVC   0(6,R3),11(R4)      JUST USE DATA, EVENT CODE                    
         LA    R3,10(R3)                                                        
         LA    R4,35(R4)           NEXT ONE                                     
*                                                                               
         MVC   3(1,R3),11(R4)      JUST USE DATA, MULTI-BRAND                   
         LA    R3,10(R3)                                                        
         LA    R4,35(R4)           NEXT ONE                                     
*                                                                               
         MVC   3(1,R3),11(R4)      JUST USE DATA, NO BRAND                      
         GOTO1 REPORT                                                           
         B     AGYC3               KEEP DOING IT UNTIL FINISHED                 
*                                                                               
RUNL     DS    0H                                                               
         B     EXIT                                                             
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
         SPACE 2                                                                
PP96WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
FRSTSW   DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055PPREP9602 05/01/02'                                      
         END                                                                    
