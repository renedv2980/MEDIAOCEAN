*          DATA SET SPOM20102  AT LEVEL 029 AS OF 05/01/02                      
*PHASE SP0102,+0                                                                
         TITLE 'SPOMB0102 - OM STATION BUCKETS'                                 
SP0102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SP01**                                                       
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         A     R9,=F'4096'                                                      
         USING SPWORKD,RA,R9                                                    
         USING STABUCK,R7                                                       
         USING BILLREC,R6                                                       
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCBILL                                                    
         BE    PRBILL                                                           
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
REQF     DS    0H                                                               
         L     R7,ADBUY                                                         
         XC    0(256,R7),0(R7)                                                  
         B     EXIT                                                             
*                                                                               
REQL     DS    0H                                                               
         L     R6,ADBILL                                                        
         MVI   BKEY,X'FF'                                                       
         MVC   AREC,ADBUY                                                       
         L     R5,AREC                                                          
         CLI   0(R5),0             TEST ANY RECORD TO FLUSH                     
         BE    EXIT                                                             
         B     PR6                                                              
*                                                                               
PRBILL   DS    0H                                                               
         CLC   CLT,=C'EG '         SKIP THREE CLIENTS                           
         BE    EXIT                                                             
         CLC   CLT,=C'IS '         SKIP THREE CLIENTS                           
         BE    EXIT                                                             
         CLC   CLT,=C'ZS '         SKIP THREE CLIENTS                           
         BE    EXIT                                                             
         GOTO1 GETBILL                                                          
         L     R6,ADBILL                                                        
         L     R7,ADBUY                                                         
         ST    R7,AREC                                                          
*                                                                               
         CLC   BKEY(8),OLDKEY                                                   
         BE    PR8                                                              
*                                                                               
         OC    OLDKEY,OLDKEY                                                    
         BZ    PR7                                                              
*                                  FINISH OLD RECORD                            
PR6      DS    0H                                                               
         L     R5,AREC                                                          
         BAS   RE,DMPREC                                                        
         CLI   RCWRITE,C'Y'                                                     
         BNE   PR7                                                              
         MVC   WORK(64),KEY                                                     
         GOTO1 ADD                                                              
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
*                                                                               
PR7      DS    0H                  START NEW REC                                
         CLI   BKEY,X'FF'          END                                          
         BE    EXIT                                                             
         MVC   OLDKEY,BKEY                                                      
         XC    STABUCK(256),STABUCK                                             
*                                                                               
         MVC   STABKCOD,=X'0E01'                                                
         MVC   STABKAM(3),KEY+1    AGY/M/CLT                                    
         MVC   STABKPRD,BPRD                                                    
         MVC   STABKEST,KEY+7                                                   
         MVC   STABKMKT,=H'9998'                                                
         MVC   STABKSTA,=X'D6B5A4'   STATION ZZZZ                               
         MVC   STABLEN,=Y(24)                                                   
*                                                                               
PR8      DS    0H                  ADD AN ELEM                                  
         LA    R2,STABELEM                                                      
PR8D     DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    PR9                                                              
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     PR8D                                                             
*                                                                               
PR9      DS    0H                                                               
         LA    R4,WORK                                                          
         XC    WORK,WORK                                                        
         USING STABELEM,R4                                                      
*                                                                               
         MVC   STABELEM(2),=X'0E12'                                             
         MVC   STABPER,BKEYYSRV                                                 
         GOTO1 DATCON,DMCB,(0,BDATE),(2,STABBDT)                                
         MVC   STABINV,BKEYINV                                                  
         PACK  DUB,BAMT                                                         
         CVB   R0,DUB                                                           
         STCM  R0,15,STABGRS                                                    
         MVC   STABNET,BNET                                                     
         XC    STABSPTS,STABSPTS                                                
*                                                                               
         GOTO1 RECUP,DMCB,(C'S',STABUCK),WORK,(R2)                              
         B     EXIT                                                             
*                                                                               
PR60     DS    0H                                                               
         B     EXIT                                                             
         SPACE 3                                                                
DMPREC   NTR1                                                                   
         CLI   QOPT1,C'Y'                                                       
         BNE   EXIT                                                             
         SR    R2,R2                                                            
         ICM   R2,3,13(R5)         LENGTH                                       
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
         GOTO1 REPORT                                                           
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
         LTORG                                                                  
         SPACE 3                                                                
OLDKEY   DC   XL13'00'                                                          
*                                                                               
         DC    4096X'00'                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPGENSTAB                                                      
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029SPOM20102 05/01/02'                                      
         END                                                                    
