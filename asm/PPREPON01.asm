*          DATA SET PPREPON01  AT LEVEL 012 AS OF 08/09/00                      
*PHASE PPON01A                                                                  
         TITLE 'PPOG01 - OGILVY PRINT TAPE CONVERSION SPECS'                    
PPON01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PRTFILE                                                   
         FSPEC UPDATE,PRTDIR                                                    
*                                                                               
         PSPEC H1,76,AGYNAME                                                    
         PSPEC H2,76,AGYADD                                                     
         PSPEC H3,76,PAGE                                                       
         PSPEC H1,2,REQUESTOR                                                   
         PSPEC H2,2,RUN                                                         
         PSPEC H1,47,C'BUY CREATION FROM TAPE'                                  
         PSPEC H2,47,C'----------------------'                                  
         PSPEC H5,1,C'TYP CLI  PRD   EST   PUB'                                 
         PSPEC H6,1,C'--- ---  ---   ---   ---'                                 
         PSPEC H5,40,C'INS DATE   COL    INCH    RATE        JOB'               
         PSPEC H6,40,C'--------   ----   -----   ---------   ---'               
         PSPEC H5,87,C'LINE NO.          COST'                                  
         PSPEC H6,87,C'--------      -----------'                               
         PSPEC H7,1,C' '                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012PPREPON01 08/09/00'                                      
         END                                                                    
