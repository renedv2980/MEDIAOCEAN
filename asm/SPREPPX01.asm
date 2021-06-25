*          DATA SET SPREPPX01  AT LEVEL 020 AS OF 08/29/00                      
*PHASE SPPX01A                                                                  
         TITLE 'SPPX01 - HDS MP AGENCY DATA EXTRACT'                            
SPPX01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC OPEN,DEMFILES                                                    
         FSPEC READ,BUYS                                                        
         FSPEC GET,MARKETS                                                      
*                                                                               
         SSPEC H1,43,C'AGENCY MEDIA PLANNING EXTRACT'                           
         SSPEC H2,43,C'-----------------------------'                           
*                                                                               
         SSPEC H2,1,MEDIA                                                       
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H3,1,CLIENT                                                      
         SSPEC H4,1,PGROUP                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
*                                                                               
         SSPEC H3,41,PERIOD                                                     
         SSPEC H4,41,MGROUP                                                     
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H4,100,RATING                                                    
         SSPEC H5,100,BOOK                                                      
         SSPEC H6,100,PAGE                                                      
         SSPEC H6,111,REPORT                                                    
*                                                                               
         DC    X'00'                                                            
*                                                                               
         DC    C'REQLST='                                                       
         DC    CL25'3404ADEMO MENU'                                             
         DC    X'00'                                                            
 END                                                                            
