*          DATA SET ACINF09    AT LEVEL 002 AS OF 03/05/92                      
*PHASE T60509A,*,NOAUTO                                                         
T60509   CSECT                                                                  
*                                                                               
*              EXPLANATORY TEXT USED BY HELP RECORD TYPE IN ACCOUNTS            
*              INFO PROGRAM                                                     
*                                                                               
         DC    CL30'KEY'                                                        
         DC    CL29' '                                                          
         DC    CL30'1 THIS DEFINES THE START AND E'                             
         DC    CL29'ND POINTS FOR THE ENQUIRY.'                                 
         DC    CL30'2 THE FORMAT IS - ''STARTKEY,EN'                            
         DC    CL29'DKEY''.'                                                    
         DC    CL30'3 SOME STARTKEY ELEMENTS ARE M'                             
         DC    CL29'ANDATORY FOR SOME RECORD '                                  
         DC    CL30'  TYPES, BUT ENDKEY IS ALWAYS'                              
         DC    CL29'OPTIONAL.'                                                  
         DC    CL30'4 FOR DETAILS, ENTER A RECORD'                              
         DC    CL29'TYPE WITH ''HELP'' IN THE KEY'                              
         DC    CL30'  FIELD. VALID KEYS FOR THIS R'                             
         DC    CL29'ECORD TYPE WILL THEN BE SHOWN'                              
         DC    CL30'  WITH OPTIONAL KEY ELEMENTS I'                             
         DC    CL29'N PARENTHESES.'                                             
*                                                                               
         DC    CL30'FILTERS'                                                    
         DC    CL29' '                                                          
         DC    CL30'1 THESE DEFINE ONE OR MORE SEL'                             
         DC    CL29'ECTION CRITERIA.'                                           
         DC    CL30'2 THE FORMAT IS - ''KEYWORD=X,K'                            
         DC    CL29'EYWORD=Y'' ETC.'                                            
         DC    CL30'  WHERE EACH KEYWORD MAY BE AB'                             
         DC    CL29'BREVIATED TO A STANDARD,'                                   
         DC    CL30'  2-CHARACTER FORM.'                                        
         DC    CL29' '                                                          
         DC    CL30'3 TO SPECIFY A NEGATIVE FILTER'                             
         DC    CL29' (SELECT IF NOT X), INSERT AN'                              
         DC    CL30'  ASTERISK AFTER ''='', EG ''KEYW'                          
         DC    CL29'ORD=*X''.'                                                  
         DC    CL30'4 FOR A RECORD TO BE DISPLAYED'                             
         DC    CL29', ALL THE FILTER CRITERIA'                                  
         DC    CL30'  MUST BE SATISFIED.'                                       
         DC    CL29' '                                                          
         DC    CL30'5 FOR DETAILS, ENTER A RECORD'                              
         DC    CL29'TYPE WITH ''HELP'' IN THE'                                  
         DC    CL30'  FILTERS FIELD.'                                           
         DC    CL29' '                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACINF09   03/05/92'                                      
         END                                                                    
