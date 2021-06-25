*          DATA SET ACREPGT01  AT LEVEL 025 AS OF 08/17/00                      
*PHASE ACGT01A                                                                  
         TITLE 'ACGT01 - GST REPORT SPECS'                                      
ACGT01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         FSPEC READ,HISTORY                                                     
         RSPEC MAXLINES,55                                                      
         SPACE 2                                                                
         SPROG 0,1,2,3,4,5                                                      
         ACDEF H1,2,RUN                                                         
         ACDEF H1,84,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H3,2,COMPANY                                                     
         ACDEF H3,84,REQUESTOR                                                  
         ACDEF H4,84,AC#PERD,8,L                                                
         ACDEF H4,95,PERIOD                                                     
         ACDEF H5,95,MOSFILT                                                    
         ACDEF F1,2,REQDETS                                                     
*                                                                               
         SPROG 0,1                                                              
         ACDEF H7,2,AC#ACC,7,L                                                  
         ACDEF H7,19,AC#NAME,4,L                                                
         ACDEF H8,2,AC#ACC,7,LU                                                 
         ACDEF H8,19,AC#NAME,4,LU                                               
*                                                                               
         SPROG 0                                                                
         ACDEF H7,60,AC#GSTBA,14,R                                              
         ACDEF H7,75,AC#VATO,14,R                                               
         ACDEF H7,94,AC#PCT,10,R                                                
         ACDEF H8,60,AC#CLICO,14,RU                                             
         ACDEF H8,75,AC#VATO,14,RU                                              
         ACDEF H8,94,AC#PCT,10,RU                                               
*                                                                               
         SPROG 1                                                                
         ACDEF H7,60,AC#GSTBA,14,R                                              
         ACDEF H7,75,AC#VATI,14,R                                               
         ACDEF H7,94,AC#PCT,10,R                                                
         ACDEF H8,60,AC#GSTBA,14,RU                                             
         ACDEF H8,75,AC#VATI,14,RU                                              
         ACDEF H8,94,AC#PCT,10,RU                                               
*                                                                               
         SPROG 2,3                                                              
*        ACDEF H5,2,AC#ACC,7,L                                                  
         ACDEF H7,2,AC#CTR,13,L                                                 
         ACDEF H7,17,AC#REFC,8,L                                                
         ACDEF H7,27,AC#DATE,6,L                                                
         ACDEF H7,39,AC#BATRF,12,L                                              
         ACDEF H7,52,AC#ANL,8,L                                                 
         ACDEF H8,2,AC#CTR,13,LU                                                
         ACDEF H8,17,AC#REFC,8,LU                                               
         ACDEF H8,27,AC#DATE,6,LU                                               
         ACDEF H8,39,AC#BATRF,12,LU                                             
         ACDEF H8,52,AC#ANL,8,LU                                                
*                                                                               
         SPROG 4,5                                                              
*        ACDEF H5,2,AC#ACC,7,L                                                  
         ACDEF H7,2,AC#CTR,13,L                                                 
         ACDEF H8,2,AC#CTR,13,LU                                                
*                                                                               
         SPROG 2,4                                                              
         ACDEF H7,60,AC#GSTBA,14,R                                              
         ACDEF H7,75,AC#VATO,14,R                                               
         ACDEF H7,94,AC#PCT,10,R                                                
         ACDEF H8,60,AC#GSTBA,14,RU                                             
         ACDEF H8,75,AC#VATO,14,RU                                              
         ACDEF H8,94,AC#PCT,10,RU                                               
*                                                                               
         SPROG 3,5                                                              
         ACDEF H7,60,AC#GSTBA,14,R                                              
         ACDEF H7,75,AC#VATI,14,R                                               
         ACDEF H7,94,AC#PCT,10,R                                                
         ACDEF H8,60,AC#GSTBA,14,RU                                             
         ACDEF H8,75,AC#VATI,14,RU                                              
         ACDEF H8,94,AC#PCT,10,RU                                               
*                                                                               
         SPROG 0,2,4                                                            
         ACDEF H4,55,AC#OTPT,14,C                                               
*                                                                               
         SPROG 1,3,5                                                            
         ACDEF H4,55,AC#INP,14,C                                                
*                                                                               
*        ACDDEQUS                                                               
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025ACREPGT01 08/17/00'                                      
         END                                                                    
