*          DATA SET SPREPX201  AT LEVEL 003 AS OF 08/29/00                      
*PHASE SPX201A                                                                  
         TITLE 'SPX201 - NEW SPOT UN-BILLING'                                   
SPX201   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTFILE                                                   
*                                                                               
         SPROG 0,10                                                             
         SSPEC H1,54,C'INTERPUBLIC BILLING EXTRACT'                             
         SSPEC H2,54,C'---------------------------'                             
*                                                                               
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H4,98,REPORT                                                     
         SSPEC H5,98,PAGE                                                       
*                                                                               
         SSPEC H2,1,MEDIA                                                       
         SSPEC H4,1,CLIENT                                                      
*                                                                               
         SPROG 0                                                                
         SSPEC H8,10,C'INVOICE   PRD   EST   PERIOD   MGR  STATION'             
         SSPEC H9,10,C'-------   ---   ---   ------   ---  -------'             
*                                                                               
         SPROG 10                                                               
*                                                                               
         SSPEC H8,20,C'PRD   MKT  STATION    EST-LIN'                           
         SSPEC H9,20,C'---   ---  -------    -------'                           
*                                                                               
         SPROG 0,10                                                             
         SSPEC H8,70,C'GROSS'                                                   
         SSPEC H9,70,C'-----'                                                   
*                                                                               
         SSPEC H8,89,C'NET'                                                     
         SSPEC H9,89,C'---'                                                     
*                                                                               
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPX201 08/29/00'                                      
         END                                                                    
