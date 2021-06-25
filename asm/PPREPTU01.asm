*          DATA SET PPREPTU01  AT LEVEL 002 AS OF 06/03/96                      
*PHASE PPTU01A,+0                                                               
         TITLE 'PPTU01 - TEARSHEET UPLOAD'                                      
PPTU01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PRTFILE                                                   
*                                                                               
         PSPEC H1,46,C'PRINTPAK TEARSHEET UPLOAD REPORT'                        
         PSPEC H2,46,C'--------------------------------'                        
         PSPEC H1,103,AGYNAME                                                   
         PSPEC H2,103,AGYADD                                                    
         PSPEC H4,103,REPORT                                                    
         PSPEC H4,123,PAGE                                                      
         PSPEC H5,103,RUN                                                       
         PSPEC H9,1,C'AGY M CLT PRD EST PUBLICATION'                            
         PSPEC H10,1,C'--- - --- --- --- -----------'                           
         PSPEC H8,56,C'***  UPDATES  ***'                                       
         PSPEC H9,37,C'INS DATE-LN        STAT  DATA   COMM'                    
         PSPEC H10,37,C'------------       ----  ----   ----'                   
         PSPEC H9,96,C'** MESSAGE **'                                           
         PSPEC H10,96,32C'-'                                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PPREPTU01 06/03/96'                                      
         END                                                                    
