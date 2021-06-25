*          DATA SET PPREPEB01  AT LEVEL 007 AS OF 12/20/05                      
*PHASE PPEB01A                                                                  
         TITLE 'PPEB01 - PRINT BILLNG EDI -  SPECS'                             
PPEB01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CLIENTS                                                     
         FSPEC UPDATE,PRTFILE                                                   
*                                                                               
         PSPEC H1,1,MEDIA                                                       
         PSPEC H2,1,REQUESTOR                                                   
*                                                                               
         PSPEC H1,46,C'PRINTPAK EDI BILL TRANSFER'                              
         PSPEC H2,46,C'--------------------------'                              
         PSPEC H3,48,PERIOD                                                     
*                                                                               
         PSPEC H7,001,CL40'         INVOICE    BILLING   INVOICE   '            
         PSPEC H8,001,CL40'M CLI     NUMBER      DATE      DATE    '            
         PSPEC H9,001,CL40'- ---  ----------  --------  --------  -'            
*                                                                               
         PSPEC H7,041,CL40' EDI                                    '            
         PSPEC H8,041,CL40' DATE    ADDRESS                        '            
         PSPEC H9,041,CL40'-------  ----------------------------   '            
*                                                                               
         PSPEC H1,100,AGYNAME                                                   
         PSPEC H2,100,AGYADD                                                    
         PSPEC H4,100,PAGE                                                      
         PSPEC H5,100,REPORT                                                    
         PSPEC H6,100,RUN                                                       
         DC    X'0000'                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007PPREPEB01 12/20/05'                                      
         END                                                                    
