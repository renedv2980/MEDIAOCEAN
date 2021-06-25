*          DATA SET ACREPMC01  AT LEVEL 002 AS OF 08/17/00                      
*PHASE AC0701A                                                                  
         TITLE 'SPECS FOR AUTH/RECON/MANUAL CHEQUE'                             
AC0701   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,RECOVER                                                     
         ACDEF MAXLINES,60                                                      
*                                                                               
         ACDEF SPROG,0,1,2,3                                                    
         ACDEF H1,1,RUN                                                         
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,100,PAGE                                                      
         ACDEF H7,2,AC#ACC,14                                                   
         ACDEF H8,2,AC#ACC,14,LU                                                
         ACDEF H7,34,AC#DATE,5                                                  
         ACDEF H8,34,AC#DATE,5,LU                                               
         ACDEF H7,43,AC#REF,6                                                   
         ACDEF H8,43,AC#REF,6,LU                                                
         ACDEF H7,51,AC#SUBR,6                                                  
         ACDEF H8,51,AC#SUBR,6,LU                                               
         ACDEF H7,65,AC#AMT,6                                                   
         ACDEF H8,65,AC#AMT,6,LU                                                
*                                                                               
         ACDEF SPROG,1,2                                                        
         ACDEF H7,18,AC#CTRA,14                                                 
         ACDEF H8,18,AC#CTRA,14,LU                                              
*                                                                               
         ACDEF SPROG,3                                                          
         ACDEF H4,86,C'*='                                                      
         ACDEF H4,88,AC#RVRSL,16                                                
         ACDEF H7,18,AC#CHKC,14                                                 
         ACDEF H8,18,AC#CHKC,14,LU                                              
*                                                                               
         ACDEF LANG,0                                                           
         ACDEF SPROG,1                                                          
         ACDEF H1,35,C'AUTHORISATION LIST'                                      
         ACDEF H2,35,C'------------------'                                      
         ACDEF SPROG,2                                                          
         ACDEF H1,35,C'RECONCILIATION LIST'                                     
         ACDEF H2,35,C'-------------------'                                     
         ACDEF SPROG,3                                                          
         ACDEF H1,35,C'MANUAL CHEQUE LIST'                                      
         ACDEF H2,35,C'------------------'                                      
*                                                                               
         ACDEF LANG,3                                                           
         ACDEF SPROG,1                                                          
         ACDEF H1,37,C'GEPRUEFTE EINGANGSRECHNUNGEN'                            
         ACDEF H2,37,C'----------------------------'                            
         ACDEF SPROG,2                                                          
         ACDEF H1,37,C'BANK-ABSTIMMUNGS-LISTE'                                  
         ACDEF H2,37,C'----------------------'                                  
         ACDEF SPROG,3                                                          
         ACDEF H1,37,C'MAN.SCHECKZAHLUNGS-LISTE'                                
         ACDEF H2,37,C'------------------------'                                
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPMC01 08/17/00'                                      
         END                                                                    
