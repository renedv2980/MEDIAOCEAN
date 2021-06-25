*          DATA SET SPREP4901  AT LEVEL 034 AS OF 12/01/92                      
*PHASE SP4901A,+0                                                               
         TITLE 'SP49-01 - SPOTPAK AGENCY SUMMARY '                              
SP4901   CSECT                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
         SPACE 1                                                                
         FSPEC USE,SP0003                                                       
         RSPEC REQUEST,NOREP                                                    
         SPACE 1                                                                
*                                  ALL MODES                                    
         SPROG 10,30                                                            
         SPACE 1                                                                
         SSPEC H1,1,MEDIA                                                       
         SSPEC H4,99,PAGE                                                       
         SSPEC H5,1,REPORT                                                      
         SPACE 2                                                                
         SPROG 10,30                                                            
         SPACE 1                                                                
         SSPEC H3,53,C'PERIOD FROM'                                             
         SSPEC H3,72,C'TO'                                                      
         SPACE 2                                                                
         SPACE 2                                                                
         SPACE 1                                                                
         SSPEC H7,1,C'                     PCT   '                              
         SSPEC H7,68,C'------------------G R O S S  A M O U N T--------X        
               ----------'                                                      
         SSPEC H8,1,C'          PERIOD    CLRD'                                 
         SSPEC H8,51,C'ORDERED          CLEARED        UNCLEARED       X        
                   BILLED         BILLABLE'                                     
         SSPEC H9,1,C'          ------    ----'                                 
         SSPEC H9,51,C'-------          -------        ---------       X        
                   ------         --------'                                     
         SPACE 2                                                                
         SPACE 2                                                                
*                                  AGENCY SUMMARY                               
         SPROG 10                                                               
         SPACE 1                                                                
         SSPEC H1,55,C'AGENCY BROADCAST SUMMARY'                                
         SSPEC H2,55,C'------------------------'                                
         SPACE 2                                                                
         SPROG 10,20,50,60                                                      
         SPACE 1                                                                
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SPACE 2                                                                
         SPACE 2                                                                
*                                  DDS SUMMARY                                  
         SPROG 30                                                               
         SPACE 1                                                                
         SSPEC H1,61,C'DDS SUMMARY'                                             
         SSPEC H1,99,C'DONOVAN DATA SYSTEMS, INC.'                              
         SSPEC H2,61,C'-----------'                                             
         SPACE 2                                                                
         DC    X'0000'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034SPREP4901 12/01/92'                                      
         END                                                                    
