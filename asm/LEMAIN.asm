*          DATA SET LEMAIN     AT LEVEL 001 AS OF 10/27/99                      
*PHASE LEMAIN                                                                   
         TITLE 'LEMAIN - SAMPLE LE MAIN BAL PROGRAM'                            
         PRINT NOGEN                                                            
LEMAIN   CSECT                                                                  
LEMAIN   CEEENTRY AUTO=WORKSIZE,MAIN=YES                                        
         SPACE 1                                                                
         REQUS                                                                  
         USING WORKAREA,RD                                                      
         SPACE 1                                                                
         CALL  CEEMOUT,(HELLOMSG,DEST,FBCODE),VL,MF=(E,CALLMOUT)                
         SPACE 1                                                                
         CALL  CEEMOUT,(BEFORE,DEST,FBCODE),VL,MF=(E,CALLMOUT)                  
         SPACE 1                                                                
*                                                                               
* ALLOCATE A BELOW THE LINE HEAP AND ACQUIRE STORAGE FROM IT                    
*                                                                               
         CALL  CEECRHP,(HEAPID,SIZE,INCRMENT,OPTION,FBCODE),VL,        X        
               MF=(E,CALLCRHP)                                                  
         CLC   FBCODE,CEE000                                                    
         BE    LEMAIN05                                                         
         CALL  CEEMOUT,(MEMORY,DEST,FBCODE),VL,MF=(E,CALLMOUT)                  
         B     LEMAINX                                                          
*                                                                               
LEMAIN05 CALL  CEEGTST,(HEAPID,MEMSIZE,APARM,FBCODE),VL,               X        
               MF=(E,CALLGTST)                                                  
         CLC   FBCODE,CEE000                                                    
         BE    LEMAIN10                                                         
*                                                                               
         CALL  CEEMOUT,(MEMORY,DEST,FBCODE),VL,MF=(E,CALLMOUT)                  
         B     LEMAIN50                                                         
*                                                                               
* MOVE THE STRING PARAMETER INTO THE ACQUIRED BELOW THE LINE STORAGE            
* THEN LINK TO THE DDS CODE, CREATING A NEW ENCLAVE                             
*                                                                               
LEMAIN10 L     R1,APARM                                                         
         MVC   0(L'PARM,R1),PARM                                                
         LINK  EP=LEDDS                                                         
*                                                                               
         CALL  CEEMOUT,(AFTER,DEST,FBCODE),VL,MF=(E,CALLMOUT)                   
         SPACE 1                                                                
*                                                                               
* DISCARD THE HEAP ALLOCATED BELOW THE LINE                                     
*                                                                               
LEMAIN50 CALL  CEEDSHP,(HEAPID,FBCODE),VL,MF=(E,CALLDSHP)                       
         SPACE 1                                                                
LEMAINX  CEETERM RC=0                                                           
*                                                                               
* CONSTANTS                                                                     
*                                                                               
DEST     DC    F'2'                                                             
SIZE     DC    F'4096'                                                          
INCRMENT DC    F'4096'                                                          
OPTION   DC    F'76'                                                            
MEMSIZE  DC    F'40'                                                            
CEE000   DC    3F'0'                                                            
PARM     DC    CL20'LEMAIN PARAMETER'                                           
*                                                                               
HELLOMSG DC    Y(L'HELLOSTR)                                                    
HELLOSTR DC    C'LANGUAGE ENVIRONMENT CALLED ME!'                               
*                                                                               
BEFORE   DC    Y(L'BEFOREST)                                                    
BEFOREST DC    C'BEFORE HEAP AND MEMORY ALLOCATION'                             
*                                                                               
AFTER    DC    Y(L'AFTERST)                                                     
AFTERST  DC    C'AFTER DDS MODULE CALL'                                         
*                                                                               
MEMORY   DC    Y(L'MEMORYST)                                                    
MEMORYST DC    C'COULD NOT ACQUIRE STORAGE'                                     
*                                                                               
PPA      CEEPPA ,                                                               
         SPACE 2                                                                
WORKAREA DSECT                                                                  
         ORG   *+CEEDSASZ                                                       
         SPACE 1                                                                
APARM    DS    A                                                                
HEAPID   DS    F                                                                
CALLMOUT CALL  ,(,,),VL,MF=L                                                    
CALLCRHP CALL  ,(,,,,),VL,MF=L                                                  
CALLGTST CALL  ,(,,,),VL,MF=L                                                   
CALLDSHP CALL  ,(,),VL,MF=L                                                     
FBCODE   DS    3F                                                               
         SPACE 1                                                                
         DS    0D                                                               
WORKSIZE EQU   *-WORKAREA                                                       
         CEEDSA ,                                                               
         CEECAA ,                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001LEMAIN    10/27/99'                                      
         END   LEMAIN                                                           
