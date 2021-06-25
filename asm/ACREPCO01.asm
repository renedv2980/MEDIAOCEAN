*          DATA SET ACREPCO01  AT LEVEL 025 AS OF 08/16/00                      
*PHASE ACCO01A,*                                                                
ACCO01   TITLE '- SPECS FOR CLOSE OUT PROGRAM REPORT'                           
ACCO01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,OFACCOUNTS                                                  
         ACDEF READ,TRANSACTIONS                                                
         ACDEF GETOPT,N                                                         
*&&UK*&& ACDEF MAXLINES,60                                                      
*&&US*&& ACDEF MAXLINES,58                                                      
         ACDEF SPROG,0,1,2,3,4,5                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H1,52,AC#CLSOT,30                                                
         ACDEF H2,52,AC#CLSOT,30,LU                                             
         ACDEF H1,90,REPORT                                                     
         ACDEF H1,104,PAGE                                                      
         SPACE 1                                                                
         SPROG 0,1,2,3,4                                                        
         ACDEF H3,2,AC#CPY,10                                                   
         ACDEF H4,2,AC#LGR,10                                                   
         SPROG 1,2                                                              
         ACDEF H7,2,AC#ACCCD,17                                                 
         ACDEF H7,21,AC#ACCN,36                                                 
         SPROG 2                                                                
         ACDEF H7,57,AC#OFFC,2                                                  
         SPROG 1,2                                                              
         ACDEF H7,61,AC#BALBF,12,R                                              
         ACDEF H7,75,AC#DRS,12,R                                                
         ACDEF H7,89,AC#CRS,12,R                                                
         ACDEF H7,103,AC#BALCF,12,R                                             
         SPROG 3                                                                
         ACDEF H7,2,AC#ACCCD,17                                                 
         ACDEF H7,21,AC#CTRA,17                                                 
         SPROG 4                                                                
         ACDEF H7,2,AC#ACCCD,15                                                 
         ACDEF H7,19,AC#OFFC,2                                                  
         ACDEF H7,23,AC#CTRA,15                                                 
         SPROG 3,4                                                              
         ACDEF H7,40,AC#DATE,8                                                  
         ACDEF H7,49,AC#REF,6                                                   
         ACDEF H7,56,AC#SUBR,2                                                  
         ACDEF H7,59,AC#DRS,12,R                                                
         ACDEF H7,73,AC#CRS,12,R                                                
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025ACREPCO01 08/16/00'                                      
         END                                                                    
