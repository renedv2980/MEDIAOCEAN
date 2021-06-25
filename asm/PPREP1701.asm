*          DATA SET PPREP1701  AT LEVEL 005 AS OF 08/09/00                      
*PHASE PP1701A                                                                  
         TITLE 'PP1702 - PRINTPAK AUT RATE CHG-WORKER FILE READ'                
PP1701   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,119,PAGE                                                      
         PSPEC H5,98,RUN                                                        
         PSPEC H1,49,C'AUTO RATE CHANGE - WORKER FILE READ'                     
         PSPEC H2,49,C'-----------------------------------'                     
         PSPEC H3,49,PERIOD                                                     
         PSPEC H7,2,C'FILE  AGENCY MEDIA CLIENT     OLD GROSS          X        
                NEW GROSS              CHANGE      INSERTIONS'                  
         PSPEC H8,2,C'----  ------ ----- ------     ---------          X        
                ---------              ------      ----------'                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005PPREP1701 08/09/00'                                      
         END                                                                    
