*          DATA SET SPREPTR01  AT LEVEL 008 AS OF 08/29/00                      
*PHASE SPTR01A                                                                  
         TITLE 'SPTR01 --  TRAFFIC BILLING - SPECS'                             
SPTR01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H1,2,REQUESTOR                                                   
*                                                                               
         SSPEC H1,30,C'T R A F F I C  B I L L I N G  R E P O R T'               
*                                                                               
         SSPEC H2,30,C'-------------  -------------  -----------'               
*                                                                               
         SSPEC H1,77,AGYNAME                                                    
*                                                                               
         SSPEC H2,77,AGYADD                                                     
*                                                                               
         SSPEC H3,37,C'PERIOD'                                                  
*                                                                               
         SSPEC H4,77,REPORT                                                     
*                                                                               
         SSPEC H5,77,PAGE                                                       
*                                                                               
         SSPEC H6,3,C'AGENCY   MEDIA   CLT   CLIENT NAME'                       
         SSPEC H7,3,C'------   -----   ---   -----------------'                 
 END                                                                            
