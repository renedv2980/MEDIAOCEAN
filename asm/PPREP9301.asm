*          DATA SET PPREP9301  AT LEVEL 005 AS OF 07/18/16                      
*PHASE PP9301A                                                                  
         TITLE 'PP9301  GREY BILLING INTERFACE TAPE'                            
PP9301   CSECT                                                                  
         FSPEC READ,BILLS                                                       
         PRINT NOGEN                                                            
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0                                                                
         PSPEC H1,1,MEDIANAME                                                   
         PSPEC H1,20,CL36'GREY BILLING INTERFACE TAPE CREATION'                 
         PSPEC H1,70,AGYNAME                                                    
         PSPEC H2,20,36C'-'                                                     
         PSPEC H2,70,AGYADD                                                     
         PSPEC H3,23,PERIOD                                                     
         PSPEC H3,70,RUN                                                        
         PSPEC H4,70,REPORT                                                     
         PSPEC H4,95,PAGE                                                       
         PSPEC H6,8,C'CLT INVOICE PRD  EST   MON SVC        GROSS      X        
               COMMISSION      CASH DISC.             NET'                      
         PSPEC H7,8,C'--- ------- ---  ---   -------   ------------   -X        
               ------------   -------------   -------------'                    
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005PPREP9301 07/18/16'                                      
         END                                                                    
