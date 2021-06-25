*          DATA SET ACREP5501  AT LEVEL 011 AS OF 08/08/06                      
*PHASE AC5501B                                                                  
         TITLE 'SPECS FOR CHECKS'                                               
AC5501   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF READ,HOLD                                                        
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF NOREP,REQDETS                                                    
         ACDEF NOSUM,REQDETS                                                    
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H1,28,AC#TOTRN,18,L      TOTALS FOR RUN                          
         ACDEF H1,49,REPORT                                                     
         ACDEF H2,49,RUN                                                        
*---------------------------------------------------------------*               
*        SPROG 2-8 IS FORMATED FOR ENGLISH AND CANADIAN         *               
*        CHECKS                                                 *               
*---------------------------------------------------------------*               
*              SPOT                                                             
*---------------------------------------------------------------*               
         ACDEF SPROG,2                                                          
         ACDEF H1,3,AC#CLINT,10,L       CLIENT                                  
         ACDEF H1,24,AC#PRO,10,L        PRODUCT                                 
         ACDEF H1,36,AC#PERD,10,L       PERIOD                                  
         ACDEF H1,51,AC#INV,10,L        INVOICE                                 
         ACDEF H2,51,AC#NUM,10,L        NUMBER                                  
         ACDEF H1,64,AC#NET,10,R        NET                                     
         ACDEF H2,64,AC#AMT,10,R        AMOUNT                                  
         ACDEF H1,75,AC#NET,10,R        NET                                     
         ACDEF H2,75,AC#TOTAL,10,R      TOTAL                                   
*---------------------------------------------------------------*               
*              PRINT                                                            
*---------------------------------------------------------------*               
         ACDEF SPROG,3                                                          
         ACDEF H1,2,AC#PUBLN,20,L       PUBLICATION                             
         ACDEF H2,2,AC#CLIP,20,L        CLIENT/PRODUCT                          
         ACDEF H1,23,AC#PERD,10,L       PERIOD                                  
         ACDEF H1,37,AC#INV,10,L        INVOICE                                 
         ACDEF H2,37,AC#NUM,10,L        NUMBER                                  
         ACDEF H1,49,AC#NET,10,R        NET                                     
         ACDEF H2,49,AC#PAYBL,10,R      PAYABLE                                 
         ACDEF H1,63,AC#CASH,10,R       CASH                                    
         ACDEF H2,63,AC#DISS,10,R       DISCOUNT                                
         ACDEF H1,74,AC#NETLS,10,R      NET - LESS                              
         ACDEF H2,74,AC#CSHDS,10,R      CASH DISCOUNT                           
*---------------------------------------------------------------*               
*              PRODUCTION/EXPENSE                                               
*---------------------------------------------------------------*               
         ACDEF SPROG,4                                                          
         ACDEF H1,2,AC#CLCAT,30,L       CLIENT/CATEGORY                         
         ACDEF H2,2,AC#DESC,30,L        DESCRIPTION                             
         ACDEF H1,43,AC#INV,7,L         INVOICE                                 
         ACDEF H2,43,AC#NUM,7,L         NUMBER                                  
         ACDEF H1,50,AC#NET,9,R         NET                                     
         ACDEF H2,50,AC#PAYBL,9,R       PAYABLE                                 
         ACDEF H1,63,AC#CASH,10,R       CASH                                    
         ACDEF H2,63,AC#DISS,10,R       DISCOUNT                                
         ACDEF H1,75,AC#NETLS,10,R      NET LESS                                
         ACDEF H2,75,AC#CSHDS,10,R      CASH DISCOUNT                           
*---------------------------------------------------------------*               
*              COKE EXPENDITURE                                                 
*---------------------------------------------------------------*               
         ACDEF SPROG,5                                                          
         ACDEF H1,2,AC#PRO,10,L         PRODUCT                                 
         ACDEF H2,2,AC#MED,10,L         MEDIA                                   
         ACDEF H1,32,AC#INV,15,F        ---INVOICE---                           
         ACDEF H2,32,AC#NUM,6,L         NUMBER                                  
         ACDEF H2,39,AC#DATE,8,C                DATE                            
         ACDEF H1,50,AC#NET,9,R         NET                                     
         ACDEF H2,50,AC#PAYBL,9,R       PAYABLE                                 
         ACDEF H1,64,AC#CASH,10,R       CASH                                    
         ACDEF H2,64,AC#DISS,10,R       DISCOUNT                                
         ACDEF H1,75,AC#NETLS,10,R      NET LESS                                
         ACDEF H2,75,AC#CSHDS,10,R      CASH DISCOUNT                           
*---------------------------------------------------------------*               
*              AOR                                                              
*---------------------------------------------------------------*               
         ACDEF SPROG,6                                                          
         ACDEF H1,3,AC#CLINT,10,L       CLIENT                                  
         ACDEF H1,25,AC#PRO,10,L        PRODUCT                                 
         ACDEF H1,38,AC#EST,8,C         ESTIMATE                                
         ACDEF H2,38,AC#NUM,8,C         NUMBER                                  
         ACDEF H1,48,AC#INV,8,C         INVOICE                                 
         ACDEF H2,48,AC#DATE,8,C        DATE                                    
         ACDEF H1,57,AC#ADVG,6,C        ADVERTISING                             
         ACDEF H2,57,AC#MTH,6,C         MONTH                                   
         ACDEF H1,65,AC#INV,7,L         INVOICE                                 
         ACDEF H2,65,AC#NUM,7,L         NUMBER                                  
         ACDEF H1,77,AC#AMT,10,R        AMOUNT                                  
*---------------------------------------------------------------*               
*              UNWIRED REP                                                      
*---------------------------------------------------------------*               
         ACDEF SPROG,7                                                          
         ACDEF H1,3,AC#CLINT,10,L       CLIENT                                  
         ACDEF H1,25,AC#EST,8,L         ESTIMATE                                
         ACDEF H2,25,AC#NUM,8,L         NUMBER                                  
         ACDEF H1,39,AC#ADVG,8,C        ADVERTISING                             
         ACDEF H2,39,AC#PERD,8,L        PERIOD                                  
         ACDEF H1,49,AC#INV,7,L         INVOICE                                 
         ACDEF H2,49,AC#NUM,7,L         NUMBER                                  
         ACDEF H1,60,AC#NET,10,R        NET                                     
         ACDEF H2,60,AC#AMT,10,R        AMOUNT                                  
         ACDEF H1,75,AC#TOTAL,10,R      TOTAL                                   
*---------------------------------------------------------------*               
*              PRODUCTION/EXPENSE - FORMAT OPTION 1                             
*---------------------------------------------------------------*               
         ACDEF SPROG,8                                                          
         ACDEF H1,3,AC#INV,8,L          INVOICE                                 
         ACDEF H2,3,AC#DATE,8,L         DATE                                    
         ACDEF H1,13,AC#RSINV,15,L      INVOICE NUMBER                          
         ACDEF H2,13,C'---------------'                                         
         ACDEF H1,30,AC#DESC,20,C       DESCRIPTION                             
         ACDEF H2,30,C'--------------------'                                    
         ACDEF H1,50,AC#GROSS,10,R      GROSS                                   
         ACDEF H2,50,AC#AMT,10,R        AMOUNT                                  
         ACDEF H1,62,AC#DISS,10,R       DISCOUNT                                
         ACDEF H1,75,AC#NET,10,R        NET                                     
         ACDEF H2,75,AC#AMT,10,R        AMOUNT                                  
*---------------------------------------------------------------*               
*        SPROG 9-15 ARE FOR FRENCH FORMATTED CHECKS             *               
*---------------------------------------------------------------*               
*              FRENCH SPOT                                                      
*---------------------------------------------------------------*               
         ACDEF SPROG,9                                                          
         ACDEF H1,3,AC#CLINT,10,L       CLIENT                                  
         ACDEF H1,24,AC#PRO,10,L        PRODUCT                                 
         ACDEF H1,36,AC#PERD,10,L       PERIOD                                  
         ACDEF H2,51,AC#INV,10,L        INVOICE                                 
         ACDEF H1,51,AC#NUM,10,L        NUMBER                                  
         ACDEF H2,64,AC#NET,10,R        NET                                     
         ACDEF H1,64,AC#AMT,10,R        AMOUNT                                  
         ACDEF H2,75,AC#NET,10,R        NET                                     
         ACDEF H1,75,AC#TOTAL,10,R      TOTAL                                   
*---------------------------------------------------------------*               
*              FRENCH PRINT                                                     
*---------------------------------------------------------------*               
         ACDEF SPROG,10                                                         
         ACDEF H1,2,AC#PUBLN,20,L       PUBLICATION                             
         ACDEF H2,2,AC#CLIP,20,L        CLIENT/PRODUCT                          
         ACDEF H1,23,AC#PERD,10,L       PERIOD                                  
         ACDEF H2,37,AC#INV,10,L        INVOICE                                 
         ACDEF H1,37,AC#NUM,10,L        NUMBER                                  
         ACDEF H1,49,AC#NET,10,R        NET                                     
         ACDEF H2,49,AC#PAYBL,10,R      PAYABLE                                 
         ACDEF H1,63,AC#CASH,10,R       CASH                                    
         ACDEF H2,63,AC#DISS,10,R       DISCOUNT                                
         ACDEF H1,74,AC#NETLS,10,R      NET - LESS                              
         ACDEF H2,74,AC#CSHDS,10,R      CASH DISCOUNT                           
*---------------------------------------------------------------*               
*              FRENCH PRODUCTION/EXPENSE                                        
*---------------------------------------------------------------*               
         ACDEF SPROG,11                                                         
         ACDEF H1,2,AC#CLCAT,30,L       CLIENT/CATEGORY                         
         ACDEF H2,2,AC#DESC,30,L        DESCRIPTION                             
         ACDEF H2,43,AC#INV,7,L         INVOICE                                 
         ACDEF H1,43,AC#NUM,7,L         NUMBER                                  
         ACDEF H1,50,AC#NET,9,R         NET                                     
         ACDEF H2,50,AC#PAYBL,9,R       PAYABLE                                 
         ACDEF H1,63,AC#CASH,10,R       CASH                                    
         ACDEF H2,63,AC#DISS,10,R       DISCOUNT                                
         ACDEF H1,75,AC#NETLS,10,R      NET LESS                                
         ACDEF H2,75,AC#CSHDS,10,R      CASH DISCOUNT                           
*---------------------------------------------------------------*               
*              FRENCH COKE EXPENDITURE                                          
*---------------------------------------------------------------*               
         ACDEF SPROG,12                                                         
         ACDEF H1,2,AC#PRO,10,L         PRODUCT                                 
         ACDEF H2,2,AC#MED,10,L         MEDIA                                   
         ACDEF H2,32,AC#INV,15,F        ---INVOICE---                           
         ACDEF H1,32,AC#NUM,6,L         NUMBER                                  
         ACDEF H1,39,AC#DATE,8,C                DATE                            
         ACDEF H1,50,AC#NET,9,R         NET                                     
         ACDEF H2,50,AC#PAYBL,9,R       PAYABLE                                 
         ACDEF H1,64,AC#CASH,10,R       CASH                                    
         ACDEF H2,64,AC#DISS,10,R       DISCOUNT                                
         ACDEF H1,75,AC#NETLS,10,R      NET LESS                                
         ACDEF H2,75,AC#CSHDS,10,R      CASH DISCOUNT                           
*---------------------------------------------------------------*               
*              FRENCH AOR                                                       
*---------------------------------------------------------------*               
         ACDEF SPROG,13                                                         
         ACDEF H1,3,AC#CLINT,10,L       CLIENT                                  
         ACDEF H1,25,AC#PRO,10,L        PRODUCT                                 
         ACDEF H1,38,AC#EST,8,C         ESTIMATE                                
         ACDEF H2,38,AC#NUM,8,C         NUMBER                                  
         ACDEF H1,48,AC#INV,8,C         INVOICE                                 
         ACDEF H2,48,AC#DATE,8,C        DATE                                    
         ACDEF H1,57,AC#ADVG,6,C        ADVERTISING                             
         ACDEF H2,57,AC#MTH,6,C         MONTH                                   
         ACDEF H2,65,AC#INV,7,L         INVOICE                                 
         ACDEF H1,65,AC#NUM,7,L         NUMBER                                  
         ACDEF H1,77,AC#AMT,10,R        AMOUNT                                  
*---------------------------------------------------------------*               
*              FRENCH UNWIRED REP                                               
*---------------------------------------------------------------*               
         ACDEF SPROG,14                                                         
         ACDEF H1,3,AC#CLINT,10,L       CLIENT                                  
         ACDEF H1,25,AC#EST,8,L         ESTIMATE                                
         ACDEF H2,25,AC#NUM,8,L         NUMBER                                  
         ACDEF H1,39,AC#ADVG,8,C        ADVERTISING                             
         ACDEF H2,39,AC#PERD,8,L        PERIOD                                  
         ACDEF H2,49,AC#INV,7,L         INVOICE                                 
         ACDEF H1,49,AC#NUM,7,L         NUMBER                                  
         ACDEF H2,60,AC#NET,10,R        NET                                     
         ACDEF H1,60,AC#AMT,10,R        AMOUNT                                  
         ACDEF H1,75,AC#TOTAL,10,R      TOTAL                                   
*---------------------------------------------------------------*               
*              FRENCH PRODUCTION/EXPENSE - FORMAT OPTION 1                      
*---------------------------------------------------------------*               
         ACDEF SPROG,15                                                         
         ACDEF H2,3,AC#INV,8,L          INVOICE                                 
         ACDEF H1,3,AC#DATE,8,L         DATE                                    
         ACDEF H1,13,AC#RSINV,15,L      INVOICE NUMBER                          
         ACDEF H2,13,C'---------------'                                         
         ACDEF H1,30,AC#DESC,20,C       DESCRIPTION                             
         ACDEF H2,30,C'--------------------'                                    
         ACDEF H2,50,AC#GROSS,10,R      GROSS                                   
         ACDEF H1,50,AC#AMT,10,R        AMOUNT                                  
         ACDEF H1,62,AC#DISS,10,R       DISCOUNT                                
         ACDEF H2,75,AC#NET,10,R        NET                                     
         ACDEF H1,75,AC#AMT,10,R        AMOUNT                                  
*                                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACREP5501 08/08/06'                                      
         END                                                                    
         PRINT OFF                                                              
**PAN#1  CSECT                                                                  
         END                                                                    
