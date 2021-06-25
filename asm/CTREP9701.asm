*          DATA SET CTREP9701  AT LEVEL 003 AS OF 08/22/00                      
*PHASE CT9701A                                                                  
         TITLE 'CT9701 - SPECS FOR DISK ALLOCATION REPORT'                      
CT9701   CSECT                                                                  
         ASPEC H1,4,RUN                                                         
         ASPEC H1,46,C'DISK ALLOCATION REPORT'                                  
         ASPEC H2,46,C'----------------------'                                  
         ASPEC H1,99,PAGE                                                       
         ASPEC H4,5,C'START     END    NUMBER   START      END '                
         ASPEC H5,5,C'TRACK    TRACK   TRACKS   CCCHH     CCCHH'                
         ASPEC H6,5,C'-----    -----   ------   -----     -----'                
         ASPEC H4,50,C'FILE     EXTENT'                                         
         ASPEC H5,50,C'NAME      SEQ    SYSNUM'                                 
         ASPEC H6,50,C'----     ------  ------'                                 
*                                                                               
         RSPEC REQUEST,NOREP                                                    
 END                                                                            
