*          DATA SET SPOM20101  AT LEVEL 001 AS OF 02/18/87                      
*          DATA SET SP0M20101  AT LEVEL 010 AS OF 01/30/87                      
*PHASE SP0101,+0                                                                
         TITLE 'SPONSOR TEST SPECS'                                             
SP0101   CSECT                                                                  
         FSPEC USE,SPBT03                                                       
         FSPEC READ,BILLS                                                       
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         PSPEC H1,2,REPORT                                                      
         PSPEC H1,40,C'SPONSOR TEST PROGRAM'                                    
         PSPEC H2,40,20C'-'                                                     
         PSPEC H1,80,PAGE                                                       
         PSPEC H1,100,REQUESTOR                                                 
         PSPEC H2,100,AGYNAME                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPOM20101 02/18/87'                                      
         END                                                                    
