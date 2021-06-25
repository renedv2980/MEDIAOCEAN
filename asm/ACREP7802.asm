*          DATA SET ACREP7802  AT LEVEL 006 AS OF 08/16/00                      
*PHASE AC7802A                                                                  
         TITLE 'MEDIA CATEGORY LISTING'                                         
AC7802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC7802                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         CLI   MODE,REQFRST                                                     
         BNE   MCL16                                                            
         MVC   PAGE(2),=H'1'                                                    
         MVI   FORCEHED,C'Y'                                                    
         EJECT                                                                  
*        PRINT MEDIA CATEGORIES                                                 
         SPACE 3                                                                
MCL10    L     R2,ADCOMP                                                        
         AH    R2,DATADISP                                                      
         USING ACMEDIAD,R2                                                      
         SPACE 2                                                                
MCL12    CLI   0(R2),X'00'                                                      
         BE    MCL16                                                            
         CLI   0(R2),X'11'                                                      
         BNE   MCL14                                                            
         MVI   SPACING,2                                                        
         MVC   P+13(1),ACMDCODE                                                 
         MVC   P+24(15),ACMDDESC                                                
         MVC   P+44(14),ACMDCOMM+1                                              
*&&UK*&& MVC   P+69(14),ACMDVTAC+1                                              
*&&UK*&& MVC   P+84(4),ACMDRSET                                                 
*&&UK*&& MVC   P+98(1),ACMDANAL                                                 
*&&US*&& MVC   P+66(4),ACMDRSET                                                 
*&&US*&& MVC   P+80(1),ACMDANAL                                                 
         GOTO1 ACREPORT                                                         
         SPACE 2                                                                
MCL14    SR    R3,R3                                                            
         IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     MCL12                                                            
         SPACE 2                                                                
MCL16    XIT1                                                                   
         EJECT                                                                  
*        ACGENMODES                                                             
*        ACREPWORKD                                                             
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREP7802 08/16/00'                                      
         END                                                                    
