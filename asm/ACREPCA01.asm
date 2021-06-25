*          DATA SET ACREPCA01  AT LEVEL 048 AS OF 03/16/99                      
*PHASE ACCA01A,+0                                                               
         TITLE 'SPECS FOR TIME/COST BUCKET ALLOCATION'                          
ACCA01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,HISTORY                                                     
*                                                                               
         ACDEF SPROG,0,1,2,3,5,7,8,9,10,11,12,13,14,15,16,17,18,19,20, X        
               21                                                               
         ACDEF H1,45,AC#TMCSA,20                                                
         ACDEF H2,86,REQUESTOR                                                  
         ACDEF H1,119,PAGE                                                      
         ACDEF H2,45,AC#TMCSA,20,LU                                             
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,2,RUN                                                         
         ACDEF H2,2,COMPANY                                                     
*                                                                               
         ACDEF SPROG,1,2,14,15,17,18,19,20                                      
         ACDEF H3,86,C'POOL TYP='                                               
         ACDEF H4,86,C'ORIGIN  ='                                               
         ACDEF H5,86,C'POOL KEY='                                               
         ACDEF H6,86,AC#CTR,6,L                                                 
         ACDEF H6,94,C'='                                                       
         ACDEF H8,86,AC#SCM,6,L                                                 
         ACDEF H8,93,C'#'                                                       
*                                                                               
         ACDEF SPROG,1,15,18,20                                                 
         ACDEF H11,2,AC#CLI,6                                                   
         ACDEF H11,59,AC#YTD,3                                                  
         ACDEF H11,62,C'-1'                                                     
         ACDEF H11,74,AC#YTD,3                                                  
         ACDEF H11,88,AC#YTD,3                                                  
         ACDEF H11,91,C'-1'                                                     
         ACDEF H11,94,C'OVERHD',R                                               
         ACDEF H11,102,AC#YTD,3                                                 
         ACDEF H11,106,C'OVERHD',R                                              
         ACDEF H11,116,AC#PSTG,7,R                                              
         ACDEF H11,124,AC#AMT,3,R                                               
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H11,65,AC#DIRCO,8,R                                              
         ACDEF H11,78,AC#DIRCO,8,R                                              
         ACDEF SPROG,15                                                         
         ACDEF H11,64,AC#INCM,8,R                                               
         ACDEF H11,78,AC#INCM,8,R                                               
         ACDEF SPROG,18                                                         
         ACDEF H11,64,AC#HOURS,8,R                                              
         ACDEF H11,78,AC#HOURS,8,R                                              
         ACDEF SPROG,20                                                         
         ACDEF H11,64,C'DIR+DPT '                                               
         ACDEF H11,78,C'DIR+DPT '                                               
*                                                                               
         ACDEF SPROG,2,14,17,19                                                 
         ACDEF H11,2,AC#CLI,6                                                   
*        ACDEF H12,2,AC#CLI,6,LU                                                
         ACDEF H11,47,AC#YTD,3                                                  
         ACDEF H11,50,C'-1'                                                     
         ACDEF H11,63,AC#YTD,3                                                  
         ACDEF H11,85,AC#AMT,3,R                                                
         ACDEF H11,90,AC#YTD,3                                                  
         ACDEF H11,93,C'-1'                                                     
         ACDEF H11,96,AC#IDRCO,8,R                                              
         ACDEF H11,106,AC#YTD,3                                                 
         ACDEF H11,110,AC#IDRCO,8,R                                             
         ACDEF H11,120,AC#PSTG,7,R                                              
         ACDEF H11,128,AC#AMT,3,R                                               
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H11,53,AC#DIRCO,8,R                                              
         ACDEF H11,67,AC#DIRCO,8,R                                              
         ACDEF H11,77,AC#PSTG,7,R                                               
         ACDEF SPROG,14                                                         
         ACDEF H11,53,AC#INCM,8,R                                               
         ACDEF H11,67,AC#INCM,8,R                                               
         ACDEF H11,77,AC#MTHLY,7,R                                              
         ACDEF SPROG,17                                                         
         ACDEF H11,53,AC#HOURS,8,R                                              
         ACDEF H11,67,AC#HOURS,8,R                                              
         ACDEF H11,77,AC#MTHLY,7,R                                              
         ACDEF SPROG,19                                                         
         ACDEF H11,53,C'DIR+DPT '                                               
         ACDEF H11,67,C'DIR+DPT '                                               
         ACDEF H11,77,AC#PSTG,7,R                                               
*                                                                               
         ACDEF SPROG,3                                                          
         ACDEF H5,2,AC#OFFDS,20             OFFICE-DEPT SUMMARY                 
         ACDEF H10,2,AC#OFF,6                                                   
         ACDEF H10,12,AC#DPT,4                                                  
         ACDEF H11,2,AC#OFF,6,LU                                                
         ACDEF H11,12,AC#DPT,4,LU                                               
         ACDEF H10,26,AC#DIR,7,R                                                
         ACDEF H11,29,AC#TIME,4                                                 
         ACDEF H10,34,AC#ABSD,10,R                                              
         ACDEF H11,36,AC#IDRTI,8                                                
         ACDEF H10,46,AC#OFF,9,R                                                
         ACDEF H11,47,AC#IDRTI,8                                                
         ACDEF H10,57,AC#CRPN,9,R                                               
         ACDEF H11,58,AC#IDRTI,8                                                
         ACDEF H10,71,AC#TFOR,6,R                                               
         ACDEF H11,73,AC#TIME,4                                                 
         ACDEF H10,78,AC#ABSD,10,R                                              
         ACDEF H11,80,AC#OVHS,8                                                 
         ACDEF H10,93,AC#OTHRI,6,R                                              
         ACDEF H11,91,AC#OVHS,8                                                 
         ACDEF H10,104,AC#TFOR,6,R                                              
         ACDEF H11,104,AC#COST,6,R                                              
*                                                                               
         ACDEF SPROG,5                                                          
         ACDEF H7,2,C'* *  P O S T E D  S U M M A R Y  * *'                     
         ACDEF H8,2,C'------------------------------------'                     
*                                                                               
         ACDEF SPROG,7                                                          
         ACDEF H7,2,C'* * *   E R R O R  L I S T   * * *'                       
         ACDEF H8,2,C'----------------------------------'                       
*                                                                               
         ACDEF SPROG,8                                                          
         ACDEF H7,2,C'* * *   S O R T   L I S T   * * *'                        
         ACDEF H8,2,C'---------------------------------'                        
         ACDEF H11,2,C'ACCOUNT          CONTRA ACCOUNT   CONTRA NAME'           
         ACDEF H12,2,C'-------          --------------   -----------'           
         ACDEF H11,78,C'DEBIT        CREDIT'                                    
         ACDEF H12,78,C'-----        ------'                                    
*                                                                               
         ACDEF SPROG,9                                                          
         ACDEF H7,2,C'* * *   R E C O R D  A D D S * * *'                       
         ACDEF H8,2,C'----------------------------------'                       
         ACDEF H11,9,C'SIZE     # RECS'                                         
         ACDEF H12,9,C'----     ------'                                         
         ACDEF SPROG,10,11,12,13,16,21                                          
         ACDEF H7,2,C'* * *   W A R N I N G   * * *'                            
         ACDEF H8,2,C'-----------------------------'                            
*                                                                               
         ACDEF SPROG,10                                                         
         ACDEF H7,32,AC#ESNH,36                                                 
         ACDEF H8,32,AC#ESNH,36,LU                                              
*                                                                               
         ACDEF SPROG,11                                                         
         ACDEF H7,32,AC#NEGH,36                                                 
         ACDEF H8,32,AC#NEGH,36,LU                                              
*                                                                               
         ACDEF SPROG,12                                                         
         ACDEF H7,32,AC#PERH,36                                                 
         ACDEF H8,32,AC#PERH,36,LU                                              
*                                                                               
         ACDEF SPROG,13                                                         
         ACDEF H7,32,AC#NOSAL,36                                                
         ACDEF H8,32,AC#NOSAL,36,LU                                             
*                                                                               
         ACDEF SPROG,16                                                         
         ACDEF H7,32,AC#HRNET,36                                                
         ACDEF H8,32,AC#HRNET,36,LU                                             
*                                                                               
         ACDEF SPROG,21                                                         
         ACDEF H7,32,AC#ERQDT,36                                                
*                                                                               
**                                                                              
**   ACDDEQUS                                                                   
**                                                                              
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048ACREPCA01 03/16/99'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
