*          DATA SET ACREPBG01  AT LEVEL 028 AS OF 09/20/02                      
*PHASE ACBG01A,*                                                                
ACBG01   TITLE '- DAILY JOURNAL: BATCH RECORDS GENERATOR'                       
ACBG01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF READ,RECOVER,DELETES                                             
         ACDEF NOSUM                                                            
*&&UK*&& ACDEF MAXLINES,60                                                      
*&&US*&& ACDEF MAXLINES,58                                                      
         ACDEF SPROG,0,1,2,3,4,5                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H1,110,REPORT                                                    
         ACDEF H1,124,PAGE                                                      
         ACDEF H1,52,AC#DJBPS,45                                                
         ACDEF H2,52,AC#DJBPS,45,LU                                             
         SPACE 1                                                                
*                                                                               
         ACDEF SPROG,0,1                                                        
         ACDEF H3,2,AC#CPY,10                                                   
         ACDEF H4,2,AC#USER,10                                                  
         ACDEF H7,2,AC#BATRF,6                                                  
         ACDEF H7,9,AC#SNUMR,2                                                  
         ACDEF H7,12,AC#BATN,15                                                 
         ACDEF H7,28,AC#BATTY,18                                                
         ACDEF H7,47,AC#PRSN,8                                                  
         ACDEF H7,56,AC#ITEMS,4                                                 
         ACDEF H7,61,AC#PSTGS,7                                                 
         ACDEF H7,69,AC#BATTO,14                                                
         ACDEF H7,84,AC#DATE,8                                                  
         ACDEF H7,93,AC#EFFDT,8                                                 
         ACDEF H7,102,AC#CMTS,30                                                
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H4,18,AC#DDSSU,30,L                                              
         ACDEF H5,18,AC#DDSSU,30,LU                                             
         ACDEF M1,18,AC#SMIF,30,L                                               
         ACDEF M2,18,AC#SMIF,30,LU                                              
         ACDEF M1,87,AC#DRS,12,R                                                
         ACDEF M2,87,AC#DRS,12,RU                                               
         ACDEF M1,102,AC#CRS,12,R                                               
         ACDEF M2,102,AC#CRS,12,RU                                              
*                                                                               
         ACDEF SPROG,3                                                          
         ACDEF H4,32,AC#DDSSU,30,L                                              
         ACDEF H5,32,AC#DDSSU,30,LU                                             
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028ACREPBG01 09/20/02'                                      
         END                                                                    
