*          DATA SET CTREP5402  AT LEVEL 005 AS OF 05/01/02                      
*PHASE CT5402A                                                                  
         TITLE 'OUTPUT TYPE REPORT'                                             
CT5402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*OUTPUT*                                                       
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
         EJECT                                                                  
*              PROCESS AN OUTPUT TYPE RECORD                                    
         SPACE 3                                                                
         CLI   MODE,REQFRST                                                     
         BNE   OT2                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         B     XIT                                                              
         SPACE 2                                                                
OT2      CLI   MODE,PROCOUT                                                     
         BNE   XIT                                                              
         L     R2,ADRECORD                                                      
         USING CTOREC,R2                                                        
         MVC   P+1(L'CTOKID),CTOKID                                             
         MVI   ELCODE,X'38'                                                     
         BAS   RE,GETEL                                                         
         BNE   OT4                                                              
         USING CTOUTD,R2                                                        
         MVC   P+16(1),CTOUTCLS                                                 
         MVC   P+25(2),CTOUTPRI                                                 
         MVC   P+33(4),CTOUTPOW                                                 
         MVC   P+41(4),CTOUTCC                                                  
         MVC   P+53(1),CTOUTCPY                                                 
         MVC   P+66(1),CTOUTDIS                                                 
         MVC   P+78(1),CTOUTSEP                                                 
         MVC   P+86(6),CTOUTTAP                                                 
         SPACE 2                                                                
OT4      MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
ELCODE   DC    X'00'                                                            
         DC    X'00'                                                            
         SPACE 2                                                                
         GETEL R2,DATADISP,ELCODE                                               
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE CTREPWORKD                                                     
       ++INCLUDE CTREPMODES                                                     
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005CTREP5402 05/01/02'                                      
         END                                                                    
