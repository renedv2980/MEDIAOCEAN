*          DATA SET ACREPA201  AT LEVEL 007 AS OF 08/16/00                      
*PHASE ACA201A,+0                                                               
         TITLE 'SPECS FOR TIME/COST ALLOCATION'                                 
         PRINT NOGEN                                                            
ACA201   CSECT                                                                  
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,HISTORY                                                     
         SPACE 1                                                                
         SPROG 0,1,2,3,4,5,6                                                    
         ASPEC H1,2,RUN                                                         
         ASPEC H1,45,C'TIME/COST ALLOCATION'                                    
         ASPEC H2,45,C'--------------------'                                    
         ASPEC H1,86,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,86,REQUESTOR                                                  
         ASPEC H3,45,C'PERIOD ENDING'                                           
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H09,2,C'CLIENT/NON-CLIENT'                                       
         ASPEC H10,2,C'-----------------'                                       
         ASPEC H09,55,C'PERIOD    YTD   ADJ YTD     YTD-1'                      
         ASPEC H10,55,C' HOURS   HOURS   HOURS       COST'                      
         ASPEC H09,95,C' YTD    POSTING'                                        
         ASPEC H10,95,C'COST     AMOUNT'                                        
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H09,2,C'CLIENT'                                                  
         ASPEC H10,2,C'------'                                                  
         ASPEC H09,58,C'  YTD-1      YTD        YTD-1'                          
         ASPEC H10,58,C'DIR COST   DIR COST   OVERHEAD'                         
         ASPEC H09,91,C'   YTD      POSTING'                                    
         ASPEC H10,91,C'OVERHEAD     AMOUNT'                                    
         SPACE 1                                                                
         SPROG 2                                                                
         ASPEC H09,2,C'CLIENT'                                                  
         ASPEC H10,2,C'------'                                                  
         ASPEC H09,35,C'GROUP    YTD-1      YTD       POSTING'                  
         ASPEC H10,35,C'-----  DIR COST   DIR COST'                             
         ASPEC H09,73,C'GROUP    YTD-1      YTD       POSTING'                  
         ASPEC H10,73,C'-----  IND COST   IND COST     AMOUNT'                  
         SPACE 1                                                                
         SPROG 3                                                                
         ASPEC H5,2,C'OFFICE-DEPT SUMMARY'                                      
         ASPEC H09,2,C'OFFICE    DEPT'                                          
         ASPEC H10,2,C'------    ----'                                          
         ASPEC H09,27,C'DIRECT   ABSORBED    OFFICE      CORP  '                
         ASPEC H10,27,C'TIME     IND TIME   IND TIME   IND TIME'                
         ASPEC H09,72,C'TOTAL   ABSORBED     OTHER       TOTAL'                 
         ASPEC H10,72,C'TIME    OVERHEAD   OVERHEAD       COST'                 
         SPACE 1                                                                
         SPROG 4                                                                
         ASPEC H5,86,20C' '                                                     
         SPACE 1                                                                
         SPROG 5                                                                
         ASPEC H09,2,C'CLIENT'                                                  
         ASPEC H10,2,C'------'                                                  
         ASPEC H09,49,C'GROUP 4'                                                
         ASPEC H10,49,C'-------'                                                
         ASPEC H09,61,C'GROUP 5'                                                
         ASPEC H10,61,C'-------'                                                
         SPACE 1                                                                
         SPROG 6                                                                
         ASPEC H10,2,C'CLIENT/NON-CLIENT'                                       
         ASPEC H11,2,C'-----------------'                                       
         ASPEC H10,55,C'PERIOD    YTD   ADJ YTD     YTD-1'                      
         ASPEC H11,55,C' HOURS   HOURS   HOURS       COST'                      
         ASPEC H10,95,C' YTD    POSTING'                                        
         ASPEC H11,95,C'COST     AMOUNT'                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPA201 08/16/00'                                      
         END                                                                    
