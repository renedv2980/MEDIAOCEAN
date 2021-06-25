*          DATA SET SPREPBT01  AT LEVEL 019 AS OF 01/13/03                      
*PHASE SPBT01A                                                                  
         TITLE 'SPBT01 - SPOTPAK - BILLING INTERFACE TAPE LISTING'              
         PRINT NOGEN                                                            
SPBT01   CSECT                                                                  
         SPACE 2                                                                
         FSPEC READ,BILLS                                                       
         FSPEC USE,SPBT03                                                       
         FSPEC UPDATE,SPTFILE                                                   
*                                                                               
         SPROG 0,50                                                             
         PSPEC H1,1,MEDIA                                                       
         PSPEC H3,1,CLIENT                                                      
         PSPEC H4,1,PRODUCT                                                     
         PSPEC H5,1,ESTIMATE                                                    
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H3,98,REPORT                                                     
         PSPEC H3,122,PAGE                                                      
*                                                                               
         PSPEC  H8,1,C'        PRD      BILLNG INVOICE'                         
         PSPEC  H9,1,C'   PRD  ACCT EST PERIOD NUMBER '                         
         PSPEC H10,1,C'   --- ----- --- ------ -------'                         
*                                                                               
         PSPEC  H8,33,C'          INVOICE   DUE    BILL'                        
         PSPEC  H9,33,C'RUN DATE   DATE     DATE   TYPE'                        
         PSPEC H10,33,C'-------- -------- -------- ----'                        
*                                                                               
         SPROG 0                   NON-CANADA                                   
*                                                                               
         PSPEC  H8,67,C'                   ACTUAL'                              
         PSPEC  H9,67,C'GROSS AMOUNT     BILL AMOUNT      NET AMOUNT'           
         PSPEC H10,67,C'------------     -----------      ----------'           
*                                                                               
         PSPEC  H8,117,C'  AGENCY'                                              
         PSPEC  H9,117,C'COMMISSION'                                            
         PSPEC H10,117,C'----------'                                            
*                                                                               
         SPROG 50                  CANADA                                       
*                                                                               
         PSPEC  H8,65,C'                 ACTUAL'                                
         PSPEC  H9,65,C'GROSS AMOUNT   BILL AMOUNT    NET AMOUNT'               
         PSPEC H10,65,C'------------   -----------    ----------'               
*                                                                               
         PSPEC  H8,109,C'  AGENCY'                                              
         PSPEC  H9,109,C'COMMISSION'                                            
         PSPEC H10,109,C'----------'                                            
*                                                                               
         PSPEC  H8,123,C'  GST/PST'                                             
         PSPEC  H9,123,C'  AMOUNT  '                                            
         PSPEC H10,123,C'----------'                                            
*                                                                               
         DC    X'00'                                                            
*                                                                               
         DC    C'REQLST='                                                       
         DC    CL25'5004ASTART MOS'                                             
         DC    CL25'5404AEND MOS'                                               
         DC    CL25'6201AOPTION 1'                                              
         DC    CL25'6301AOPTION 2'                                              
         DC    CL25'6401AOPTION 3'                                              
         DC    CL25'6501AOPTION 4'                                              
         DC    CL25'6601AOPTION 5'                                              
         DC    X'00'                                                            
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019SPREPBT01 01/13/03'                                      
         END                                                                    
