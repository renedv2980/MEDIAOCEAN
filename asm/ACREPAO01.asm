*          DATA SET ACREPAO01  AT LEVEL 001 AS OF 12/09/20                      
*PHASE ACAO01A                                                                  
         TITLE 'FIND AND OPTIONALLY DELETE ORPHANED AUDIT RECORDS'              
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* JSAY 001 24JUL20 <SPEC-47804> ORPHANED AUDIT RECORDS FOR JOBS AND   *         
*                               ESTIMATES.                            *         
***********************************************************************         
ACAO01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,38,C'ORPHANED AUDIT RECORD REPORT'                            
         ACDEF H2,38,C'----------------------------'                            
         ACDEF H3,2,C'USERID '                                                  
         ACDEF H4,2,C'-------'                                                  
         ACDEF H3,11,C'  JOB/EST/ORD  '                                         
         ACDEF H4,11,C'---------------'                                         
         ACDEF H3,30,C'AUDREC DA'                                               
         ACDEF H4,30,C'---------'                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPAO01 12/09/20'                                      
         END                                                                    
