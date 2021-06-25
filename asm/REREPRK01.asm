*          DATA SET REREPRK01  AT LEVEL 002 AS OF 08/31/00                      
*          DATA SET REREPRK01  AT LEVEL 001 AS OF 05/19/97                      
*PHASE RERK01A                                                                  
         TITLE 'REREPRK01 - KRG RANKING REPORT HEADINGS'                        
**********************************************************************          
*                                                                    *          
*        REREPRK01 -- KRG RANKING REPORT HEADINGS                    *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* 15APR1997 (JRD) -- INITIAL ENTRY                                   *          
*                      TO BAD ASPEC DOESN'T LIKE EQUATES             *          
*                                                                    *          
**********************************************************************          
RERK01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         SPROG 0,1,2,3,4,5,6,7,8,9,10                                           
         ASPEC H01,002,C'REP:'                                                  
         ASPEC H01,049,C'CUMULATIVE OWNER BILLING REPORT'                       
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,054,PERIOD                                                   
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         ASPEC H03,002,C'OFFICE:'                                               
         ASPEC H03,032,C'**** CURRENT YEAR ****'                                
         ASPEC H03,089,C'***** PRIOR YEAR *****'                                
         SPACE 1                                                                
         ASPEC H04,032,C'** AS OF:           **'                                
         ASPEC H04,089,C'** AS OF:           **'                                
*                                                                               
         ASPEC H06,001,C'RANK'                                                  
*                                                                               
         ASPEC H06,006,C'     ** OWNER **      '                                
*                                                                               
         ASPEC H05,029,C' CURRENT  '                                            
         ASPEC H06,029,C' BILLING  '                                            
*                                                                               
         ASPEC H05,040,C'VERT.'                                                 
         ASPEC H06,040,C'  %  '                                                 
*                                                                               
         ASPEC H05,046,C' CURRENT  '                                            
         ASPEC H06,046,C'  CUME    '                                            
*                                                                               
         ASPEC H05,057,C'% OF '                                                 
         ASPEC H06,057,C'CUME '                                                 
*                                                                               
         ASPEC H06,082,C'RANK'                                                  
*                                                                               
         ASPEC H05,087,C'  PRIOR   '                                            
         ASPEC H06,087,C' BILLING  '                                            
*                                                                               
         ASPEC H05,098,C'VERT.'                                                 
         ASPEC H06,098,C'  %  '                                                 
*                                                                               
         ASPEC H05,104,C'  PRIOR   '                                            
         ASPEC H06,104,C'  CUME    '                                            
*                                                                               
         ASPEC H05,116,C'% OF '                                                 
         ASPEC H06,116,C'CUME '                                                 
*                                                                               
*                                                                               
       ++INCLUDE REREPRKEQU                                                     
* > > > > > > > > >  > > END OF REREPRK01 < < < < < < < < < < < < <             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002REREPRK01 08/31/00'                                      
         END                                                                    
