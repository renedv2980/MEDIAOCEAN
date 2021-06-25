*          DATA SET PPREP9201  AT LEVEL 025 AS OF 07/20/07                      
*PHASE PP9201A,+0,NOAUTO                                                        
         TITLE 'ESTIMATE CLOSEOUT PSPECS'                                       
PP9201   CSECT                                                                  
         PRINT NOGEN                                                            
         RSPEC REQUEST,NOREP                                                    
         FSPEC UPDATE,PRTDIR                                                    
         FSPEC UPDATE,PRTFILE                                                   
         FSPEC READ,BILLS                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,PUBS                                                         
         SPACE 2                                                                
         SPROG 10,20,21,30,40,90                                                
         PSPEC H1,2,MEDIA                                                       
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,RUN                                                        
         PSPEC H5,98,REPORT                                                     
         PSPEC H5,121,PAGE                                                      
         PSPEC H2,51,PERIOD                                                     
         SPACE 2                                                                
         SPROG 10                                                               
         PSPEC H1,51,C'PRINTPAK CLOSEOUT BILL REPORT'                           
         PSPEC H2,2,CLIENT                                                      
         PSPEC H7,26,C'INVOICE'                                                 
         PSPEC H8,2,C'PRODUCT   EST   MONTH    NUMBER'                          
         PSPEC H9,2,C'-------   ---   -----    ------'                          
         PSPEC H8,36,C'BILLED AMOUNT  UNREVERSED AMOUNT'                        
         PSPEC H9,36,C'-------------  -----------------'                        
         PSPEC H8,70,C'BILL TYPE  COMMENTS'                                     
         PSPEC H9,70,C'---------  --------'                                     
         SPACE 2                                                                
         SPROG 20,21                                                            
         PSPEC H1,51,C'PRINTPAK CLOSEOUT BUY REPORT'                            
         PSPEC H2,2,CLIENT                                                      
         PSPEC H7,2,C'INSERT'                                                   
         PSPEC H7,56,C'GROSS LESS'                                              
         PSPEC H8,3,C'DATE      EST-LIN GROSS ORDERED   CASH DISCOUNT'          
         PSPEC H9,3,C'----      ------- -------------   -------------'          
         PSPEC H8,53,C'CASH DISCOUNT             PAID'                          
         PSPEC H9,53,C'-------------             ----'                          
         PSPEC H8,94,C'UNPAID          BILLED        UNBILLED'                  
         PSPEC H9,94,C'------          ------        --------'                  
         SPACE 2                                                                
         SPROG 20                                                               
         PSPEC H3,2,PRODUCT                                                     
         SPROG 30                                                               
         PSPEC H1,51,C'PRINTPAK CLOSEOUT ESTIMATE SUMMARY'                      
         PSPEC H8,3,C'CLIENT  PRODUCT  ESTIMATE  ESTIMATE NAME'                 
         PSPEC H9,3,C'------  -------  --------  -------------'                 
         PSPEC H7,53,C'ESTIMATE  ESTIMATE'                                      
         PSPEC H8,53,C' START      END      COMMENTS'                           
         PSPEC H9,53,C'--------  --------   --------'                           
         SPACE 2                                                                
         SPROG 40                                                               
         PSPEC H1,51,C'PRINTPAK CLOSEOUT AGENCY SUMMARY'                        
         PSPEC H7,10,C'GROSS ORDERED GROSS ORDERED'                             
         PSPEC H7,54,C'GROSS LESS'                                              
         PSPEC H8,2,C'MONTH  INSERT MONTHS   BILL MONTHS'                       
         PSPEC H9,2,C'-----  --------------  -----------'                       
         PSPEC H8,39,C'CASH DISCOUNT CASH DISCOUNT'                             
         PSPEC H9,39,C'------------- -------------'                             
         PSPEC H8,75,C'PAID'                                                    
         PSPEC H9,75,C'----'                                                    
         PSPEC H8,87,C'UNPAID'                                                  
         PSPEC H9,87,C'------'                                                  
         PSPEC H8,101,C'BILLED       UNBILLED'                                  
         PSPEC H9,101,C'------       --------'                                  
         SPACE 2                                                                
         SPROG 90                                                               
         PSPEC H1,51,C'PRINTPAK CLOSEOUT RUN SUMMARY'                           
         PSPEC H7,25,C'ORDERED           PAID         BILLED        PAYX        
               ABLE       BILLABLE'                                             
         PSPEC H8,25,C'-------           ----         ------        ---X        
               ----       --------'                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025PPREP9201 07/20/07'                                      
         END                                                                    
