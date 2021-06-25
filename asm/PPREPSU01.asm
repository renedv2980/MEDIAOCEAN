*          DATA SET PPREPSU01  AT LEVEL 035 AS OF 06/26/00                      
*PHASE PPSU01A,+0                                                               
         TITLE 'PPSU01 - PRINTPAK SPACE USAGES REPORT'                          
PPSU01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PRTDIR                                                    
         FSPEC UPDATE,PRTFILE                                                   
*                                                                               
         SPROG 0,10,20,30,40,50,60                                              
         PSPEC H1,045,C'PRINTPAK SPACE DESCRIPTION USAGE REPORT'                
         PSPEC H3,049,PERIOD                                                    
         PSPEC H2,045,C'---------------------------------------'                
         PSPEC H1,001,MEDIA                                                     
         PSPEC H1,098,AGYNAME                                                   
         PSPEC H2,098,AGYADD                                                    
         PSPEC H4,098,REPORT                                                    
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,098,RUN                                                       
*                                                                               
         SPROG 10                                                               
         PSPEC H08,006,C'SPACE DESCRIPTION    USAGE      NUMERIC '              
         PSPEC H09,006,C'-----------------    -----      --------'              
         PSPEC H08,046,C'VALUES              SPACE FRAGMENT VALUES AND'         
         PSPEC H09,046,C'----------------    -------------------------'         
         PSPEC H08,091,C' SPACE DESCRIPTIONS'                                   
         PSPEC H09,091,C'-----------------------------------'                   
*                                                                               
         SPROG 20                                                               
         PSPEC H08,006,C'SPACE DESCRIPTION   USAGE          SPACE '             
         PSPEC H09,006,C'-----------------   -----          ------'             
         PSPEC H08,047,C'DESCRIPTION   USAGE          SPACE '                   
         PSPEC H09,047,C'-----------   -----          ------'                   
         PSPEC H08,083,C'DESCRIPTION   USAGE'                                   
         PSPEC H09,083,C'-----------   -----'                                   
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035PPREPSU01 06/26/00'                                      
         END                                                                    
