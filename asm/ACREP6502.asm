*          DATA SET ACREP6502  AT LEVEL 005 AS OF 08/16/00                      
*PHASE AC6502A                                                                  
         TITLE 'PRINTING OF STANDARD COMMENTS'                                  
AC6502   CSECT                                                                  
         NMOD1 0,AC6502                                                         
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         CLI   MODE,REQFRST                                                     
         BNE   CM2                                                              
         MVC   PAGE(2),=H'1'                                                    
         MVI   FORCEHED,C'Y'                                                    
         B     CMXT                                                             
         EJECT                                                                  
*              PRINT A COMMENT                                                  
         SPACE 2                                                                
CM2      CLI   MODE,PROCCOMM                                                    
         BNE   CMXT                                                             
         L     R3,ADACC                                                         
         CLC   QCOMPANY,1(R3)                                                   
         BNE   CMXT                                                             
         LR    R4,R3                                                            
         AH    R3,DATADISP                                                      
         GOTO1 ACREPORT                                                         
         MVC   WORK(6),2(R4)                                                    
CM3      CLI   WORK,C' '                                                        
         BNE   CM3A                                                             
         MVC   DUB(5),WORK+1                                                    
         MVC   WORK(6),SPACES                                                   
         MVC   WORK(5),DUB                                                      
         B     CM3                                                              
CM3A     MVC   P+1(6),WORK                                                      
CM4      CLI   0(R3),0                                                          
         BE    CM8                                                              
         CLI   0(R3),X'3E'                                                      
         BNE   CM6                                                              
         USING ACOMMD,R3                                                        
         SR    R4,R4                                                            
         IC    R4,1(R3)                                                         
         SH    R4,=H'5'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+19(0),ACOMMENT                                                 
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         SPACE 1                                                                
CM6      SR    R4,R4                                                            
         IC    R4,1(R3)                                                         
         AR    R3,R4                                                            
         B     CM4                                                              
CM8      GOTO1 ACREPORT            TWO BLANKS AFTER EACH                        
         GOTO1 ACREPORT                                                         
         SPACE 3                                                                
CMXT     XIT1                                                                   
         EJECT                                                                  
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACREPWORKD                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREP6502 08/16/00'                                      
         END                                                                    
