*          DATA SET LEMAINA    AT LEVEL 004 AS OF 12/27/99                      
*PHASE LEMAINA                                                                  
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
* ACQUIRE STORAGE FROM THE BELOW THE LINE HEAP                                  
*                                                                               
         XC    HEAPID,HEAPID       USE HEAP 0 PREALLOCATED BY LE                
         CALL  CEEGTST,(HEAPID,MEMSIZE,APARM,FBCODE),VL,MF=(E,CALLGTST)         
         CLC   FBCODE,CEE000                                                    
         BE    LEMAIN10                                                         
         CALL  CEEMOUT,(MEMORY,DEST,FBCODE),VL,MF=(E,CALLMOUT)                  
         B     LEMAINX                                                          
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
* FREE STORAGE ACQUIRED FROM HEAP 0                                             
*                                                                               
LEMAIN50 CALL  CEEFRST,(APARM,FBCODE),VL,MF=(E,CALLFRST)                        
         SPACE 1                                                                
LEMAINX  CEETERM RC=0                                                           
*                                                                               
* CONSTANTS                                                                     
*                                                                               
DEST     DC    F'2'                                                             
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
CALLGTST CALL  ,(,,,),VL,MF=L                                                   
CALLFRST CALL  ,(,),VL,MF=L                                                     
FBCODE   DS    3F                                                               
         SPACE 1                                                                
         DS    0D                                                               
WORKSIZE EQU   *-WORKAREA                                                       
         CEEDSA ,                                                               
         CEECAA ,                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004LEMAINA   12/27/99'                                      
         END   LEMAIN                                                           
