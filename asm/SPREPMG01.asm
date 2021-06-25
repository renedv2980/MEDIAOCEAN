*          DATA SET SPREPMG01  AT LEVEL 004 AS OF 08/08/14                      
*PHASE SPMG01B                                                                  
         TITLE 'SPMG01 - SPOTPAK MAKEGOOD ANALYSIS - SPECS'                     
SPMG01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
*                                                                               
         SPROG 0,THRU,1                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H4,1,CLIENT                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
*        SSPEC H7,1,STATION                                                     
         SSPEC H4,1,PGROUP                                                      
*                                                                               
         SSPEC H3,51,PERIOD                                                     
         SSPEC H4,51,MGROUP                                                     
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H4,100,C'RTG SRC - (C) 20XX NIELSEN MEDIA'                       
         SSPEC H5,100,PAGE                                                      
         SSPEC H5,111,REPORT                                                    
*                                                                               
         SSPEC H1,55,C'MAKEGOOD ANALYSIS REPORT'                                
         SSPEC H2,55,C'------------------------'                                
*                                                                               
         SSPEC H9,1,C'CODE    TYPE   LINE   DATE     D SLN   TIME'              
         SSPEC H10,1,C'----    ----   ----   ----     - ---   ----'             
         SSPEC H8,54,C'MISSED                 MAKEGOOD'                         
         SSPEC H9,54,C'DOLLARS       GRPS     DOLLARS        GRPS'              
         SSPEC H10,54,C'-------       ----     --------       ----'             
         SPROG 0                                                                
         SSPEC H9,100,C'PROGRAM'                                                
         SSPEC H10,100,C'-------'                                               
         SPROG 1                                                                
         SSPEC H9,100,C'PRODUCT    PROGRAM'                                     
         SSPEC H10,100,C'-------    -------'                                    
         DC    X'00'                                                            
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPREPMG01 08/08/14'                                      
         END                                                                    
