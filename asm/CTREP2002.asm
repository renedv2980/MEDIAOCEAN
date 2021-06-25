*          DATA SET CTREP2002  AT LEVEL 005 AS OF 08/22/00                      
*PHASE CT2002A                                                                  
         TITLE 'COMMENT REPORT'                                                 
CT2002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**COMM**                                                       
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
         EJECT                                                                  
*              PROCESS A COMMENT                                                
         SPACE 3                                                                
         CLI   MODE,PROCCOM                                                     
         BNE   CM20                                                             
         L     R2,ADRECORD                                                      
         USING CTCREC,R2                                                        
         CLC   LASTCOM(7),0(R2)                                                 
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLC   LASTCOM(23),0(R2)                                                
         BE    CM2                                                              
         MVC   P,SPACES                                                         
         GOTO1 REPORT                                                           
         SPACE 2                                                                
CM2      MVC   HEAD4+9(4),CTCKUSER                                              
         LA    R3,SYSTEMS                                                       
         SPACE 2                                                                
CM4      CLI   0(R3),X'FF'                                                      
         BE    CM6                                                              
         CLC   0(1,R3),CTCKSYS                                                  
         BE    CM6                                                              
         LA    R3,8(R3)                                                         
         B     CM4                                                              
         SPACE 2                                                                
SYSTEMS  DC    CL8'SSPOT'                                                       
         DC    CL8'AACCOUNT'                                                    
         DC    CL8'PPRINT'                                                      
         DC    CL8'RREP'                                                        
         DC    CL8'MMEDLINE'                                                    
         DC    CL8'CCONTROL'                                                    
         DC    X'FF'                                                            
         DC    CL7'OTHERS'                                                      
         SPACE 2                                                                
LASTCOM  DC    CL25' '                                                          
         SPACE 2                                                                
CM6      MVC   LASTCOM,0(R2)                                                    
         MVC   HEAD5+9(7),1(R3)                                                 
         MVC   P+1(3),=C'ALL'                                                   
         SPACE 2                                                                
CM8      MVC   P+9(10),CTCKID                                                   
         EDIT  (2,CTCKSUB),(4,P+23),FILL=0                                      
         CLC   CTCKSUB,=H'1'                                                    
         BE    *+10                                                             
         MVC   P(20),SPACES                                                     
         LR    R4,R2                                                            
         AH    R4,DATADISP                                                      
         SR    R3,R3                                                            
         SPACE 2                                                                
CM10     CLI   0(R4),X'10'                                                      
         BNE   CM12                                                             
         USING CTDATD,R4                                                        
         IC    R3,CTDATLEN                                                      
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+30(0),CTDATA                                                   
         GOTO1 REPORT                                                           
         SPACE 2                                                                
CM12     CLI   0(R4),0                                                          
         BE    XIT                                                              
         IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     CM10                                                             
         SPACE                                                                  
CM20     CLI   MODE,REQFRST                                                     
         BNE   XIT                                                              
         MVC   PAGE,=H'1'                                                       
         SPACE 2                                                                
XIT      XIT1                                                                   
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTREPWORKD                                                     
       ++INCLUDE CTREPMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005CTREP2002 08/22/00'                                      
         END                                                                    
