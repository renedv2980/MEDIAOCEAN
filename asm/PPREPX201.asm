*          DATA SET PPREPX201  AT LEVEL 004 AS OF 08/09/00                      
*PHASE PPX201A                                                                  
         TITLE 'PPX201 - PRINTPAK COKE INTERFACE'                               
PPX201   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PUBFILE                                                   
         FSPEC READ,BUYS                                                        
         FSPEC GET,PUBLICATIONS                                                 
*                                                                               
         SPROG 0,10                                                             
         PSPEC H1,50,C'PRINTPAK INTERPUBLIC COKE INTERFACE'                     
         PSPEC H2,50,C'-----------------------------------'                     
*                                                                               
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H5,98,PAGE                                                       
*                                                                               
         PSPEC H2,1,MEDIA                                                       
         PSPEC H4,1,CLIENT                                                      
*                                                                               
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004PPREPX201 08/09/00'                                      
         END                                                                    
