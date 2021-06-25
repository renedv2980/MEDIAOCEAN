*          DATA SET CTREP6002  AT LEVEL 013 AS OF 05/01/02                      
*PHASE CT6002A                                                                  
         TITLE 'JCL REPORT'                                                     
CT6002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**JCL***,RR=R2                                                 
         ST    R2,RELO                                                          
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
         EJECT                                                                  
*              PROCESS A RECORD                                                 
         SPACE 3                                                                
         CLI   MODE,REQFRST                                                     
         BNE   JCL1                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         B     XIT                                                              
         SPACE 2                                                                
JCL1     CLI   MODE,PROCJCL                                                     
         BNE   XIT                                                              
         L     R2,ADRECORD                                                      
         USING CTJREC,R2                                                        
         CLI   CTJKEY+24,0                                                      
         BNE   XIT                                                              
         CLI   QOPT2,C' '          IS THIRD CHARACTER FILTER PRESENT?           
         BNH   *+14                NO                                           
         CLC   QOPT2,CTJKID+2      DOES THIRD CHARACTER MATCH?                  
         BNE   XIT                 NO                                           
         MVC   P+1(10),CTJKID                                                   
         ZAP   BOOKSEQ,=P'1'                                                    
         MVC   BOOKEY,KEY                                                       
         L     R2,=A(BUFF)                                                      
         A     R2,RELO                                                          
         MVC   0(25,R2),BOOKEY                                                  
         SPACE 2                                                                
JCL2     ST    R2,DMCB                                                          
         CLI   QOPT1,C'Y'          NEST OPTION                                  
         BE    *+8                                                              
         MVI   DMCB,1                                                           
         GOTO1 GETBOOK,DMCB,,P+22,DATAMGR                                       
         TM    8(R1),X'80'                                                      
         BO    JCL4                                                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         CLI   QOPT3,C'C'          PRINT CONTROL CARDS ONLY?                    
         BNE   JCL3                                                             
         CLC   =C'$$',P+22         YES -- IGNORE $$ AND // CARDS                
         BE    JCL2                                                             
         CLC   =C'//',P+22                                                      
         BE    JCL2                                                             
         SPACE                                                                  
JCL3     MVC   P+15(4),P+94                                                     
         GOTO1 REPORT                                                           
         B     JCL2                                                             
         SPACE 2                                                                
JCL4     GOTO1 REPORT                                                           
         BASR  RE,RF                                                            
         MVC   KEY,BOOKEY          REESTABLISH SEQUENCE                         
         L     R2,FILEC                                                         
         GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,KEY,(R2),(0,DMWORK)                   
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
BOOKSEQ  DC    PL4'0'                                                           
BOOKEY   DS    CL25                                                             
RELO     DS    A                                                                
         PRINT OFF                                                              
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CTREPWORKD                                                     
       ++INCLUDE CTREPMODES                                                     
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
BUFF     CSECT                                                                  
         DS    1200C                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013CTREP6002 05/01/02'                                      
         END                                                                    
