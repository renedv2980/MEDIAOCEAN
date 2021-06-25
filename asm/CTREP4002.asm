*          DATA SET CTREP4002  AT LEVEL 003 AS OF 08/22/00                      
*PHASE CT4002A                                                                  
         TITLE 'ERROR LISTING PROGRAM'                                          
CT4002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ERRS**                                                       
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
         EJECT                                                                  
*              HANDLE RECORDS                                                   
         SPACE 3                                                                
         CLI   MODE,REQFRST                                                     
         BNE   ER2                                                              
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         SPACE 2                                                                
ER2      CLI   MODE,PROCERR                                                     
         BNE   XIT                                                              
         L     R2,ADRECORD                                                      
         USING CTEREC,R2                                                        
         CLC   CTEKEY(24),LASTERR                                               
         BE    ER4                                                              
         MVI   FORCEHED,C'Y'                                                    
         B     ER4                                                              
         SPACE 2                                                                
LASTERR  DC    CL25' '                                                          
         SPACE 2                                                                
ER4      MVC   LASTERR,0(R2)                                                    
         EDIT  CTEKSYS,(3,HEAD4+10),ALIGN=LEFT                                  
         MVC   P,SPACES                                                         
         EDIT  CTEKNUM,(3,P+2)                                                  
         L     R4,ADDESC                                                        
         USING CTDSCD,R4                                                        
         SR    R3,R3                                                            
         IC    R3,CTDSCLEN                                                      
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+10(0),CTDSC                                                    
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         SPACE 2                                                                
XIT      XIT1                                                                   
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE CTREPWORKD                                                     
       ++INCLUDE CTREPMODES                                                     
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTREP4002 08/22/00'                                      
         END                                                                    
