*          DATA SET SPAORFIX   AT LEVEL 003 AS OF 05/16/89                      
*PHASE SP0102,+0                                                                
         TITLE 'SPAORFIX - FIX AOR RECORDS'                                     
SP0102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SP01**                                                       
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         A     R9,=F'4096'                                                      
         USING SPWORKD,RA,R9                                                    
         CLI   MODE,REQFRST                                                     
         BE    PROCESS                                                          
EXIT     XIT1                                                                   
*                                                                               
PROCESS  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D45'                                                  
         GOTO1 HIGH                                                             
         B     PR04B                                                            
PR04     DS    0H                                                               
         GOTO1 SEQ                                                              
PR04B    DS    0H                                                               
         CLC   KEY(2),KEYSAVE                                                   
         BNE   PR80                                                             
         GOTO1 HEXOUT,DMCB,KEY,P,32,=C'N'                                       
         GOTO1 REPORT                                                           
         B     PR04                                                             
*                                                                               
PR80     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
DMPREC   NTR1                                                                   
         MVI   P,0                                                              
         MVC   P2(20),DMPMSG                                                    
         L     R5,AREC                                                          
         GOTO1 MSUNPK,DMCB,7(R5),P2+35,P2+40                                    
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
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
         SPACE 2                                                                
DMPMSG   DS    CL20                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENAOR                                                       
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPAORFIX  05/16/89'                                      
         END                                                                    
