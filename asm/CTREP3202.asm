*          DATA SET CTREP3202  AT LEVEL 009 AS OF 05/01/02                      
*PHASE CT3202A                                                                  
         TITLE 'CPP PROJECTION FORMULAE'                                        
CT3202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FORM**                                                       
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
         MVI   FCRDFORM,C'Y'                                                    
         CLI   MODE,REQFRST                                                     
         BNE   PROJ1                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVI   LASTFORM,0                                                       
         MVC   PAGE,=H'1'                                                       
         B     XIT                                                              
         SPACE 2                                                                
PROJ1    CLI   MODE,PROCFORM                                                    
         BNE   XIT                                                              
         L     R2,ADRECORD                                                      
         USING CTYREC,R2                                                        
         CLC   CTYKAGY,LASTAGY                                                  
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   HEAD4+8(2),CTYKAGY                                               
         MVC   LASTAGY,CTYKAGY                                                  
         CLC   CTYKFORM,LASTFORM                                                
         BE    PROJ4                                                            
         OC    LASTFORM,LASTFORM                                                
         BZ    PROJ2                                                            
         GOTO1 REPORT                                                           
         MVC   HEAD4+8(2),CTYKAGY                                               
         BASR  RE,RF                                                            
         SPACE 2                                                                
PROJ2    MVC   P+37(1),CTYKFORM                                                 
         SPACE 2                                                                
PROJ4    MVC   LASTFORM,CTYKFORM                                                
         ZIC   R3,CTYKMON                                                       
         BCTR  R3,0                                                             
         MH    R3,=H'3'                                                         
         LA    R3,MONTHS(R3)                                                    
         MVC   P+45(3),0(R3)                                                    
         MVI   P+48,C'/'                                                        
         EDIT  (1,CTYKYEAR),(2,P+49),FILL=0                                     
         LR    R4,R2                                                            
         MVI   ELCODE,X'78'                                                     
         BAS   RE,GETEL                                                         
         BNE   PROJ6                                                            
         USING CTPFD,R4                                                         
         EDIT  (2,CTPFACT),(5,P+57)                                             
         SPACE 2                                                                
PROJ6    GOTO1 REPORT                                                           
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         SPACE 2                                                                
LASTAGY  DC    XL2'00'                                                          
LASTFORM DC    X'00'                                                            
ELCODE   DC    X'00'                                                            
         PRINT OFF                                                              
       ++INCLUDE CTREPWORKD                                                     
       ++INCLUDE CTREPMODES                                                     
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009CTREP3202 05/01/02'                                      
         END                                                                    
