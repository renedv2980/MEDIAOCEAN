*          DATA SET SPREPGW01  AT LEVEL 012 AS OF 08/29/00                      
*PHASE SPGW01A                                                                  
         PRINT NOGEN                                                            
SPGW01   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,55,C'SPOTPAK INTERAGENCY GOAL TRANSFER'                       
         SSPEC H2,55,C'---------------------------------'                       
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H4,100,PAGE                                                      
         SSPEC H4,111,REPORT                                                    
         SPROG 0                                                                
         SSPEC H5,2,C'CLT PRD EST MARKET RECORDS  DESCRIPTION'                  
         SSPEC H6,2,C'--- --- --- ------ ------   -----------'                  
         SPROG 1                                                                
         SSPEC H5,1,C' MKT  PRD  AGENCY'                                        
         SSPEC H6,1,C' ---  ---  ------'                                        
         SSPEC H5,34,C'EST '                                                    
         SSPEC H6,34,C'--- '                                                    
         SSPEC H5,43,C'PREV GOALS '                                             
         SSPEC H6,43,C'---------- '                                             
         SSPEC H5,56,C'   INPUT GOALS   CURRENT GOALS '                         
         SSPEC H6,56,C'   -----------   ------------- '                         
         SSPEC H5,88,C'       CHANGE '                                          
         SSPEC H6,88,C'       ------ '                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SPREPGW01 08/29/00'                                      
         END                                                                    
