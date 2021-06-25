*          DATA SET PPREP0301M AT LEVEL 033 AS OF 09/06/13                      
*PHASE PP0301M,+0                                                               
         TITLE 'PP0301 - PRTFIX SPECS'                                          
PP0301   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PRTDIR                                                    
         FSPEC UPDATE,PRTFILE                                                   
*                                                                               
         SPROG 0,10,20,30,40,50,60                                              
         PSPEC H1,52,C'PRINTPAK PRTFILE RECORD FIX PROGRAM'                     
         PSPEC H3,49,PERIOD                                                     
         PSPEC H2,52,C'-----------------------------------'                     
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,98,RUN                                                        
*                                                                               
         SPROG 10                                                               
         PSPEC H7,50,C'CONTRACTS COPIED FROM AGENCY BD TO AGENCY YN'            
         PSPEC H8,50,C'--------------------------------------------'            
         PSPEC H10,1,C'AGM CLT   OLD PUB          CONT'                         
         PSPEC H11,1,C'--- ---   ----------       ----'                         
         PSPEC H10,35,C'***   DATES   ***'                                      
         PSPEC H11,35,C'-----------------'                                      
         PSPEC H10,61,C'NEW PUB'                                                
         PSPEC H11,61,C'-------'                                                
*                                                                               
         SPROG 20                                                               
         PSPEC H08,1,C'AGY MED CLT  CODE  LIN  LENGTH  PUB CNT'                 
         PSPEC H09,1,C'--- --- ---  ----  ---  ------  -------'                 
         PSPEC H08,42,C'08 CNT  11 CNT'                                         
         PSPEC H09,42,C'------  ------'                                         
*                                                                               
         SPROG 30                                                               
         PSPEC H7,50,C'BUYS COPIED FROM ZONED PUB TO BASE PUB'                  
         PSPEC H8,50,C'--------------------------------------'                  
         PSPEC H10,1,C'AGM CLT PRD PUB             BUY DATE    EST'             
         PSPEC H11,1,C'--- --- --- ---             --------    ---'             
*                                                                               
         SPROG 40                                                               
         PSPEC H8,1,C' M  CLT  PUB                 CON#   LNTH'                 
         PSPEC H9,1,C' -  ---  ---                 ----   ----'                 
         PSPEC H7,43,C'X20     X21     X22'                                     
         PSPEC H8,43,C'CNT     CNT     CNT'                                     
         PSPEC H9,43,C'---     ---     ---'                                     
*                                                                               
         SPROG 50                                                               
         PSPEC H7,51,C'INSTRUCTION RECORDS FOR DELETED PUBS'                    
         PSPEC H8,51,C'------------------------------------'                    
         PSPEC H10,2,C'AGM  CLT  PRD  JOB NO  PUB CODE'                         
         PSPEC H11,2,C'---  ---  ---  ------  --------'                         
*                                                                               
         SPROG 60                                                               
         PSPEC H7,51,C'PUB LIST RECORDS WITH NO PUBS IN LIST'                   
         PSPEC H8,51,C'-------------------------------------'                   
         PSPEC H10,1,C'AG   M  CLT  LST   LN'                                   
         PSPEC H11,1,C'--   -  ---  ---   --'                                   
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033PPREP0301M09/06/13'                                      
         END                                                                    
