*          DATA SET CTREP7002  AT LEVEL 006 AS OF 05/01/02                      
*PHASE CT7002A                                                                  
         TITLE 'CT7002 - FIELD DESCRIPTION LISTING'                             
CT7002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CT7002                                                       
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
         CLI   MODE,REQFRST                                                     
         BNE   FD2                                                              
         MVC   PAGE,=H'1'                                                       
         B     XIT                                                              
         SPACE 2                                                                
FD2      CLI   MODE,PROCUSER                                                    
         BNE   XIT                                                              
         L     R2,ADRECORD                                                      
         USING CTUREC,R2                                                        
         EJECT                                                                  
*              PROCESS A USER RECORD                                            
         SPACE 3                                                                
         OC    CTUKAGY,CTUKAGY     ONLY INTERESTED IN FIELD DESC. RECS.         
         BNZ   XIT                                                              
         LR    R4,R2                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   FD6                                                              
         USING CTACTD,R4                                                        
         GOTO1 DATCON,DMCB,(3,CTACTDT),(8,HEAD5+94)                             
         SPACE 2                                                                
FD6      LR    R4,R2                                                            
         MVI   ELCODE,X'02'                                                     
         MVC   HEAD4+16(4),CTUKSYS                                              
         BAS   RE,GETEL                                                         
         BNE   FD8                                                              
         USING CTDSCD,R4                                                        
         SR    R3,R3                                                            
         IC    R3,CTDSCLEN                                                      
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   HEAD4+21(0),CTDSC                                                
         SPACE 2                                                                
FD8      LR    R4,R2                                                            
         MVI   ELCODE,X'70'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING CTFDD,R4                                                         
         SPACE 2                                                                
FD10     EDIT  (1,CTFDNUM),(2,P+15),FILL=0                                      
         SR    R3,R3                                                            
         IC    R3,CTFDLEN                                                       
         SH    R3,=H'27'                                                        
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+24(0),CTFDDESC                                                 
         MVC   P+58(1),CTFDTYPE                                                 
         OI    CTFDOTHR,X'80'                                                   
         BNO   *+8                                                              
         MVI   P+59,C'*'                                                        
         MVC   P+66(20),CTFDLIST                                                
         MVC   P+91(1),CTFDDEF                                                  
         CLI   CTFDTYPE,C'C'                                                    
         BE    FD12                                                             
         GOTO1 HEXOUT,DMCB,CTFDDEF,P+91,1,=C'SEP'                               
         CLI   CTFDTYPE,C'X'                                                    
         BE    FD12                                                             
         EDIT  (1,CTFDDEF),(3,P+91),ALIGN=LEFT                                  
         CLI   P+91,C' '                                                        
         BNE   *+8                                                              
         MVI   P+91,C'0'                                                        
         SPACE 2                                                                
FD12     CLI   CTUKPAGE,0                                                       
         BE    FD14                                                             
         MVC   HEAD5+17(4),=C'PAGE'                                             
         EDIT  (1,CTUKPAGE),(1,HEAD5+22)                                        
         SPACE 2                                                                
FD14     GOTO1 REPORT                                                           
         BAS   RE,NEXTEL                                                        
         BE    FD10                                                             
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
ELCODE   DC    X'00'                                                            
         DC    X'00'                                                            
         PRINT OFF                                                              
       ++INCLUDE CTREPWORKD                                                     
       ++INCLUDE CTREPMODES                                                     
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006CTREP7002 05/01/02'                                      
         END                                                                    
