*          DATA SET SPREPNV03  AT LEVEL 002 AS OF 08/29/00                      
*PHASE SPNV03A                                                                  
         TITLE 'SPREPNV03 - INVOICE CONTROL REPORT - CONTROLLER'                
SPNV03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPNV03                                                         
*                                                                               
         L     R9,0(R1)                                                         
         LA    RA,2048(R9)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,R9,RA                                                    
*                                                                               
FC2      GOTO1  FCNXTCLT                                                        
         BNE    EXIT                                                            
*                                                                               
         MVI   MODE,CLTFRST                                                     
         GOTO1 GO                                                               
         B     FC2                                                              
*                                                                               
EXIT     XIT1                                                                   
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPNV03 08/29/00'                                      
         END                                                                    
