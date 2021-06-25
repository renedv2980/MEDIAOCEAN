*          DATA SET PPREP02PUB AT LEVEL 032 AS OF 05/01/02                      
*PHASE PP0202P                                                                  
*INCLUDE GETINS                                                                 
*INCLUDE QSORT                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE PPBYOUT                                                                
*INCLUDE PUBEDIT                                                                
*INCLUDE GETCOST                                                                
*INCLUDE PPRTLOOK                                                               
*                                                                               
         TITLE 'PP0202 - PUB USE CHECK'                                         
         PRINT NOGEN                                                            
*                                                                               
*        TO FIND PUBS WITH "SHORT" X'14' ELEMS THAT MIGHT HAVE BEEN             
*        ADDED 9/30/91 TO 10/2/91                                               
*                                                                               
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
         MVI   RCRQONLY,C'Y'                                                    
         B     EXIT                                                             
*                                                                               
RUNL     DS    0H                                                               
*        OI    COUNT+(L'COUNT-1),15                                             
*        MVC   P(10),=C'COUNT TEST'                                             
*        UNPK  P+12(L'COUNT*2-1),COUNT                                          
         GOTO1 REPORT                                                           
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
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
PROC     DS    0H                                                               
*                                                                               
*                                  ADD CODE FOR THIS RUN HERE                   
         XC    LASTPUB,LASTPUB                                                  
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         GOTO1 HIGHPUB                                                          
         B     PROC4                                                            
*                                                                               
PROC2    DS    0H                                                               
         GOTO1 SEQPUB                                                           
         CLI   KEY,C'T'                                                         
         BH    RUNL                                                             
         TM    DMCB+8,X'80'                                                     
         BO    RUNL                                                             
         AP    COUNT,=P'1'                                                      
*                                                                               
PROC4    DS    0H                                                               
         CLI   KEY+9,X'85'                                                      
         BNE   PROC2                                                            
         GOTO1 GETNAME                                                          
         MVI   ELCODE,X'71'                                                     
         LA    R3,PUBREC    ADDRESS OF IO                                       
         LA    R2,33(R3)                                                        
         CLI   0(R2),X'71'                                                      
         BE    PROC4A5                                                          
PROC4A   BAS   RE,NEXTEL                                                        
         BNE   PROC2                                                            
PROC4A5  CLC   14(2,R2),=C'  '                                                  
         BNE   PROC4A                                                           
         MVI   SPACING,2                                                        
         BAS   RE,DMPKEY                                                        
         IC    R5,=C'0'                                                         
         GOTO1 =V(PUBEDIT),DMCB,((R5),KEY+1),(0,P+5)                            
         MVC   P+30(12),2(R2)                                                   
         BAS   RE,RPRT                                                          
         B     PROC4A                 GO DO NEXT ELEMENT                        
*                                                                               
COUNT    DC    PL5'0'                                                           
PROC4B   DS    0H                                                               
*                                                                               
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
         BAS   RE,RPRT                                                          
         B     EXIT                                                             
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         L     R5,AREC                                                          
         LA    R2,200                                                           
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
XXX      DS    4CL1000                                                          
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
SAVAGY   DS    CL2                                                              
SAVNAM   DS    CL123                                                            
ELCODE   DS    X                                                                
FRSTSW   DS    XL1                                                              
PRSW     DS    C                                                                
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
ITOT     DS    F                                                                
*                                                                               
*        PPNEWFILE,PPREPWORK,PPMODEQU FOLLOW                                    
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032PPREP02PUB05/01/02'                                      
         END                                                                    
