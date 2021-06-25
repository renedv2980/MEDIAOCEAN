*          DATA SET ACINQ0A    AT LEVEL 003 AS OF 10/28/96                      
*PHASE T6060AA,*,NOAUTO                                                         
T6060A   CSECT                                                                  
         TITLE 'ACCOUNT ENQUIRY MK2 - HELP - T6060A'                            
*                                                                               
*              EXPLANATORY TEXT USED BY 'HELP' TYPE IN THE ACCOUNT              
*              ENQUIRY PROGRAM                                                  
*                                                                               
         DC    CL30'KEY'                                                        
         DC    CL27' '                                                          
         DC    CL30'1 THIS DEFINES THE ACCOUNT OR'                              
         DC    CL27'START ACCOUNT.'                                             
         DC    CL30'2 IN ADDITION, IN MULTIPLE SCR'                             
         DC    CL27'EEN ENQUIRIES, ''NEXT'' MAY'                                
         DC    CL30'  BE INPUT IN THIS FIELD.'                                  
         DC    CL27' '                                                          
         DC    CL30'3 FOR DETAILS, ENTER A TYPE WI'                             
         DC    CL27'TH ''HELP'' IN THE KEY FIELD.'                              
         DC    CL30'  VALID KEYS FOR THIS TYPE WIL'                             
         DC    CL27'L THEN BE SHOWN WITH'                                       
         DC    CL30'  OPTIONAL KEY ELEMENTS IN PAR'                             
         DC    CL27'ENTHESES.'                                                  
*                                                                               
         DC    CL30'CONTRA-ACCOUNT'                                             
         DC    CL27' '                                                          
         DC    CL30'1 THIS IS EITHER A START OR FU'                             
         DC    CL27'LL FILTER FOR THE ENQUIRY.'                                 
*                                                                               
         DC    CL30'OPTIONS'                                                    
         DC    CL27' '                                                          
         DC    CL30'1 THESE DEFINE ONE OR MORE FIL'                             
         DC    CL27'TERS OR OTHER OPTIONS'                                      
         DC    CL30'2 THE FORMAT IS - ''KEYWORD=X,K'                            
         DC    CL27'EYWORD=Y'' ETC.'                                            
         DC    CL30'  WHERE EACH KEYWORD MAY BE AB'                             
         DC    CL27'BREVIATED TO A STANDARD,'                                   
         DC    CL30'  2-CHARACTER FORM.'                                        
         DC    CL27' '                                                          
         DC    CL30'3 TO SPECIFY A NEGATIVE FILTER'                             
         DC    CL27' (SELECT IF NOT X), INSERT'                                 
         DC    CL30'  AN ASTERISK AFTER ''='', EG'                              
         DC    CL27'''KEYWORD=*X''.'                                            
         DC    CL30'4 FOR A RECORD TO BE DISPLAYED'                             
         DC    CL27', ALL THE FILTER CRITERIA'                                  
         DC    CL30'  MUST BE SATISFIED.'                                       
         DC    CL27' '                                                          
         DC    CL30'5 FOR DETAILS, ENTER A TYPE WI'                             
         DC    CL27'TH ''HELP'' IN THIS FIELD.'                                 
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACINQ0A   10/28/96'                                      
         END                                                                    
