*          DATA SET CPREP2001  AT LEVEL 008 AS OF 09/01/00                      
*PHASE CP2001A                                                                  
         TITLE 'CPREP2001-COST PER POINT GUIDE'                                 
         PRINT NOGEN                                                            
CP2001   CSECT                                                                  
         SPROG 0,THRU,8                                                         
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,77,AGYNAME                                                    
         PSPEC H1,41,C'COST PER POINT GUIDE'                                    
         PSPEC H2,41,C'--------------------'                                    
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,77,AGYADD                                                     
         PSPEC H3,1,RANGE                                                       
         PSPEC H4,41,PERIOD                                                     
         PSPEC H4,77,REPORT                                                     
         PSPEC H4,103,PAGE                                                      
         PSPEC H9,1,C'DPT-LN'                                                   
         PSPEC H10,1,C'------'                                                  
         PSPEC H9,9,C'DATA'                                                     
         PSPEC H10,9,C'----'                                                    
         SPROG 1                                                                
         PSPEC H9,15,C'HOMES'                                                   
         PSPEC H10,15,C'-----'                                                  
         PSPEC H09,21,C'------------------WOMEN------------------'              
         PSPEC H10,21,C'TOTAL 18-34 18-49 25-49 35-49   35+   50+'              
         PSPEC H09,63,C'----MEN---- ------ADULTS-----'                          
         PSPEC H10,63,C'18-49 25-49 TOTAL 18-34 18-49'                          
         PSPEC H09,93,C'TEENS  -CHILDREN-'                                      
         PSPEC H10,93,C'-----  2-11  6-11'                                      
         SPROG 2                                                                
         PSPEC H09,15,C'-----------------------------------------'              
         PSPEC H10,15,C'12-34 15-24 18-24 18-34 18-49   18+ 25-34 '             
         PSPEC H09,57,C'---WOMEN------------------------------------'           
         PSPEC H10,57,C'25-49 25-54 25-64   25+ 35-49   35+  WORK   '           
         SPROG 3                                                                
         PSPEC H09,15,C'----------------------------------------'               
         PSPEC H10,15,C'2-11 6-11  6-17 12-17 12-24 12-34 12-49 '               
         PSPEC H09,57,C'----MEN-------------------------------------'           
         PSPEC H10,57,C'25-34 25-49 25-54 25-64   25+ 35-49   35+   '           
         PSPEC H09,101,C'---'                                                   
         PSPEC H10,101,C'50+'                                                   
         SPROG 4                                                                
         PSPEC H09,015,C'---------------------------------------- '             
         PSPEC H10,015,C'2-11  6-11  6-17 12-17 12-24 12-34 12-49 '             
         PSPEC H09,57,C'--VIEWERS-----------------------------------'           
         PSPEC H10,57,C'18-34 18-49   18+ 25-34 25-49 25-54 35-49   '           
         PSPEC H09,101,C'---------'                                             
         PSPEC H10,101,C'35+   50+'                                             
         SPROG 5                                                                
         PSPEC H09,15,C'HOMES'                                                  
         PSPEC H10,15,C'-----'                                                  
         PSPEC H09,29,C'----------VIEWERS----------'                            
         PSPEC H10,29,C'ALL   -45   +45 ADULT CHILD'                            
         PSPEC H09,65,C'MEN'                                                    
         PSPEC H10,65,C'---'                                                    
         PSPEC H09,75,C'WOMEN'                                                  
         PSPEC H10,75,C'-----'                                                  
         PSPEC H09,87,C'----HOUSEWIVES----'                                     
         PSPEC H10,87,C'+CHILD   -45   ALL'                                     
         SPROG 6                                                                
         PSPEC H9,15,C'HOMES'                                                   
         PSPEC H10,15,C'-----'                                                  
         PSPEC H09,21,C'------------WOMEN------------ '                         
         PSPEC H10,21,C'TOTAL 18-34 18-49 25-54   35+ '                         
         PSPEC H09,51,C'----MEN---- '                                           
         PSPEC H10,51,C'18-49 25-54 '                                           
         PSPEC H09,63,C'-----------ADULTS------------ '                         
         PSPEC H10,63,C'TOTAL 18-34 18-49 25-54   35+ '                         
         PSPEC H09,93,C' TEEN  KIDS '                                           
         PSPEC H10,93,C'12-17  6-11 '                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008CPREP2001 09/01/00'                                      
         END                                                                    
