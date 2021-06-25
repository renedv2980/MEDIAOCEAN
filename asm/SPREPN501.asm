*          DATA SET SPREPN501  AT LEVEL 018 AS OF 02/26/19                      
*PHASE SPN501C                                                                  
         TITLE 'SPREPN501-CANCADIAN NETWORK PTS,PRS SPECS'                      
         PRINT NOGEN                                                            
SPN501   CSECT                                                                  
         FSPEC USE,SPN503                                                       
         FSPEC OPEN,DEMFILES                                                    
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,77,AGYNAME                                                    
*        SSPEC H1,20,MGROUP                                                     
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,77,AGYADD                                                     
         SSPEC H3,1,PAGE                                                        
         SSPEC H3,77,REPORT                                                     
         SSPEC H3,30,PERIOD                                                     
         SSPEC H5,1,CLIENT                                                      
         SSPEC H6,1,PRODUCT                                                     
         SSPEC H7,1,ESTIMATE                                                    
         SSPEC H5,1,PGROUP                                                      
         SSPEC H4,77,MARKET                                                     
         DC    X'00'                                                            
*                                                                               
         DC    C'REQLST='                                                       
* THESE WILL GET REPLACED (& 10 MORE REPLACEMENTS AVAILABLE)                    
*         DC    CL25'3001ADEMO OVERRIDE ACTIVE'                                 
*         DC    CL25'5801AAFFILIATION FILTER'                                   
*         DC    CL25'5901APROGRAM TYPE FILTER'                                  
*         DC    CL25'6001ADAYPART DETAIL CON.'                                  
*         DC    CL25'6101ADAYPART MENU OVERIDE'                                 
*         DC    CL25'6201AOPTION 1'                                             
*         DC    CL25'6301AOPTION 2'                                             
*         DC    CL25'6401AOPTION 3'                                             
*         DC    CL25'6501AOPTION 4'                                             
*         DC    CL25'6601AOPTION 5'                                             
         DC    CL25'3001ADEMO OVERRIDE ACTIVE'   UNCHANGED                      
         DC    CL25'5601APOST OVERRIDE DEMOS'                                   
         DC    CL25'5801AAFFILIATION FILTER'     UNCHANGED                      
         DC    CL25'5901APROGRAM TYPE FILTER'    UNCHANGED                      
         DC    CL25'6001ADAYPART DETAIL CON.'    UNCHANGED                      
         DC    CL25'6101ADAYPART MENU OVERIDE'   UNCHANGED                      
         DC    CL25'6201AOPTION 1'               UNCHANGED                      
         DC    CL25'6301AOPTION 2'               UNCHANGED                      
         DC    CL25'6401AOPTION 3'               UNCHANGED                      
         DC    CL25'6501AOPTION 4'               UNCHANGED                      
         DC    CL25'6601AOPTION 5'               UNCHANGED                      
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018SPREPN501 02/26/19'                                      
         END                                                                    
