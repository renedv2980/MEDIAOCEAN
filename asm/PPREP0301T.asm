*          DATA SET PPREP0301T AT LEVEL 024 AS OF 10/02/98                      
*PHASE PP0301T,+0                                                               
         TITLE 'PP0301 - PRTFIX SPECS'                                          
PP0301   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PUBFILE                                                   
*                                                                               
         SPROG 0,10,20,30                                                       
         PSPEC H1,52,C'PRINTPAK PUBFILE RECORD FIX PROGRAM'                     
         PSPEC H3,49,PERIOD                                                     
         PSPEC H2,52,C'-----------------------------------'                     
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,98,RUN                                                        
         SPROG 10                                                               
         PSPEC H7,2,C'AG PUB             REP   LEVEL   TYP'                     
         PSPEC H8,2,C'-- --------------  ----  ------  ---'                     
         PSPEC H7,40,C'REP ADDRESS'                                             
         PSPEC H8,40,C'----------------------------------'                      
         PSPEC H7,77,C'AOV ADDRESS'                                             
         PSPEC H8,77,C'----------------------------------'                      
         SPROG 20                                                               
         PSPEC H7,40,C'PUBLICATIONS WITH FORD BUYS DATED JAN01/95'              
         PSPEC H7,82,C' TO THE PRESENT'                                         
         PSPEC H8,40,C'------------------------------------------'              
         PSPEC H8,82,C'---------------'                                         
         SPROG 30                                                               
         PSPEC H6,50,C'PUBLICATIONS COPIED TO NEW PUB NUMBER'                   
         PSPEC H7,50,C'-------------------------------------'                   
         PSPEC H9,2,C'COPY PUB FROM           COPY PUB TO'                      
         PSPEC H10,2,C'--------------          ---------------'                 
         PSPEC H9,54,C'RECORD NAME'                                             
         PSPEC H10,54,C'--------------------'                                   
         PSPEC H9,86,C'RECORD KEY'                                              
         PSPEC H10,86,C'--------------------'                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024PPREP0301T10/02/98'                                      
         END                                                                    
