*          DATA SET PPREP1401  AT LEVEL 037 AS OF 08/15/94                      
*PHASE PP1401A,+0                                                               
         TITLE 'PP1401 -  PRINTPAK CONTRACT LISTING SPECS'                      
PP1401   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC UPDATE,PRTFILE                                                   
*                                                                               
         SPROG 0,1,2,3,6,7,8,9,10,19,20,21,31,40                                
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,98,AGYNAME                                                    
*                                                                               
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,98,AGYADD                                                     
*                                                                               
         PSPEC H3,55,PERIOD                                                     
*                                                                               
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,119,PAGE                                                      
         PSPEC H5,98,RUN                                                        
*                                                                               
         SPROG 0,1,2,3,6,7,8,9,10,21,31,40                                      
         PSPEC H3,1,CLIENT                                                      
*                                                                               
         SPROG 0,40                                                             
         PSPEC H1,58,C'CONTRACT LISTING'                                        
         PSPEC H2,58,C'----------------'                                        
         SPROG 40                                                               
         PSPEC H4,1,DIVISION                                                    
         PSPEC H5,1,REGION                                                      
         PSPEC H6,1,DISTRICT                                                    
*                                                                               
         SPROG 1,2,3,21,31                                                      
         PSPEC H1,55,C'AUTOMATIC RATE CHANGE'                                   
         PSPEC H2,55,C'---------------------'                                   
         SPROG 6,7,9                                                            
         PSPEC H1,58,C'CONTRACT ANALYSIS'                                       
         PSPEC H2,58,C'-----------------'                                       
         SPROG 0,1,6,40                                                         
         PSPEC H7,31,C'CONTRACT'                                                
         PSPEC H7,114,C'RATE'                                                   
*                                                                               
         PSPEC H8,31,C' NUMBER     CONTRACT DATES      LEVEL      PERCEX        
               NT      RATE    DESCRIPTION        EFF. DATE'                    
*                                                                               
         PSPEC H9,31,C'--------  -----------------    --------    -----X        
               --    --------  -----------        ---------'                    
*                                                                               
         SPROG 2,3,21,31                                                        
         PSPEC H4,54,C'** INSERTION CHANGES **'                                 
         PSPEC H8,26,C'PRD    DATE    EST'                                      
         PSPEC H9,26,C'---  --------  ---'                                      
         SPROG 21,31                                                            
         PSPEC H8,67,C'                 OLD NET                   NEW NX        
               ET        CHANGE'                                                
         PSPEC H9,67,C'                 -------                   -----X        
               --        ------'                                                
         SPROG 2,3                                                              
         PSPEC H8,67,C'               OLD GROSS                 NEW GROX        
               SS        CHANGE'                                                
         PSPEC H9,67,C'               ---------                 -------X        
               --        ------'                                                
         SPROG 2,21                                                             
         PSPEC H8,46,C'SPACE          UNITS'                                    
         PSPEC H9,46,C'-----          -----'                                    
         PSPEC H7,68,C'OLD UNIT'                                                
         PSPEC H8,67,C'RATE/PREM'                                               
         PSPEC H9,67,C'---------'                                               
         PSPEC H7,94,C'NEW UNIT'                                                
         PSPEC H8,93,C'RATE/PREM'                                               
         PSPEC H9,93,C'---------'                                               
         SPROG 3,31                                                             
         PSPEC H8,46,C'DESCRIPTION'                                             
         PSPEC H9,46,C'-----------'                                             
         SPROG 7                                                                
         PSPEC H5,55,C'** INSERTION SUMMARY **'                                 
         PSPEC H6,61,C'(CONTINUED)'                                             
         SPROG 8                                                                
         PSPEC H5,56,C'** POSTING SUMMARY **'                                   
         PSPEC H6,62,C'(CONTINUED)'                                             
         SPROG 9,19                                                             
         PSPEC H5,55,C'** INSERTION SUMMARY **'                                 
         SPROG 10,20                                                            
         PSPEC H5,56,C'** POSTING SUMMARY **'                                   
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    X'01020304050617090A0B0C0D0F131400'                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037PPREP1401 08/15/94'                                      
         END                                                                    
