*          DATA SET SPREPEB01  AT LEVEL 012 AS OF 10/27/99                      
*          DATA SET SPREPEB01  AT LEVEL 040 AS OF 12/14/95                      
*PHASE SPEB01A                    NOTE "A" APPENDED TO PHASE NAME               
         TITLE 'SPEB01 - SPOT BILLNG EDI -  SPECS'                              
SPEB01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTFILE                                                   
*NOP*    FSPEC READ,BUYS                                                        
*NOP*    FSPEC READ,GOALS                                                       
*NOP*    FSPEC GET,MARKET                                                       
*NOP*    FSPEC GET,STATION                                                      
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
*                                                                               
         SSPEC H3,48,PERIOD                                                     
*                                                                               
         SSPEC H7,001,CL40'         INVOICE    BILLING   INVOICE   '            
         SSPEC H8,001,CL40'M CLI     NUMBER      DATE      DATE    '            
         SSPEC H9,001,CL40'- ---  ----------  --------  --------  -'            
*                                                                               
         SSPEC H7,041,CL40' EDI                                    '            
         SSPEC H8,041,CL40' DATE    ADDRESS                        '            
         SSPEC H9,041,CL40'-------  ----------------------------   '            
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H4,100,PAGE                                                      
         SSPEC H5,100,REPORT                                                    
         DC    X'0000'                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SPREPEB01 10/27/99'                                      
         END                                                                    
