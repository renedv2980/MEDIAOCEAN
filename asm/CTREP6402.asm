*          DATA SET CTREP6402  AT LEVEL 004 AS OF 08/22/00                      
*PHASE CT6402A                                                                  
         TITLE 'LIBRARY LIST PROGRAM'                                           
CT6402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LIBRY*                                                       
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
         EJECT                                                                  
*              PROCESS A RECORD                                                 
         SPACE 3                                                                
         CLI   MODE,PROCLIB                                                     
         BNE   XIT                                                              
         L     R2,ADRECORD                                                      
         USING CTLREC,R2                                                        
         CLC   CTLKSUB,=H'1'                                                    
         BNE   LIB2                                                             
         MVC   P,SPACES                                                         
         GOTO1 REPORT                                                           
         MVC   P+1(10),CTLKNAME                                                 
         SPACE 2                                                                
LIB2     EDIT  (2,CTLKSUB),(4,P+14),FILL=0                                      
         LR    R4,R2                                                            
         AH    R4,DATADISP                                                      
         SR    R3,R3                                                            
         SPACE 2                                                                
LIB4     CLI   0(R4),0                                                          
         BE    XIT                                                              
         CLI   0(R4),X'10'                                                      
         BNE   LIB6                                                             
         USING CTDATD,R4                                                        
         IC    R3,1(R4)                                                         
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+22(0),CTDATA                                                   
         GOTO1 REPORT                                                           
         SPACE 2                                                                
LIB6     IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     LIB4                                                             
         SPACE 2                                                                
XIT      XIT1                                                                   
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE CTREPWORKD                                                     
       ++INCLUDE CTREPMODES                                                     
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004CTREP6402 08/22/00'                                      
         END                                                                    
