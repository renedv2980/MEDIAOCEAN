*          DATA SET CTFUS01    AT LEVEL 004 AS OF 07/09/92                      
*PHASE TA0801A                                                                  
         TITLE 'CTFUS01 - PROFILE POST EDIT OVERLAY'                            
TA0801   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**PEDT*,R8,RR=RE                                     
         USING WORKD,RC            RC=A(W/S)                                    
         ST    RE,RELO                                                          
         L     R9,0(R1)            R9=A(TWA)                                    
         MVC   APREC,4(R1)                                                      
         MVC   ACOMFACS,8(R1)                                                   
*                                                                               
*                                                                               
*                                                                               
         DC    H'0'                                                             
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
*                                  I/O COMMAND LIST                             
DMRDHI   DC    C'DMRDHI '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMWRT    DC    C'DMWRT  '                                                       
*                                                                               
NOERROR  DC    XL2'FFFF'                                                        
         EJECT                                                                  
*****************************************************************               
*  TABLE ENTRIES ARE                                                            
*****************************************************************               
*****************************************************************               
         EJECT                                                                  
* DSECT TO COVER W/S                                                            
*                                                                               
WORKD    DSECT                                                                  
RELO     DS    A                                                                
DUB      DS    D                                                                
DUB2     DS    D                                                                
DMCB     DS    6F                                                               
DMCB2    DS    6F                                                               
WORK     DS    CL80                                                             
SPACES   DS    CL80                                                             
FLAG     DS    X                                                                
FLAG2    DS    X                                                                
*                                                                               
*                                                                               
FLDH     DS    D                                                                
FLD      DS    CL80                                                             
FADR     DS    A                                                                
ERRMSGN  DS    XL2                 GETTXT ERROR MESSAGE NUMBER                  
FNDX     DS    X                                                                
MSG      DS    CL60                                                             
*                                                                               
ACOMFACS DS    V                                                                
APREC    DS    V                                                                
KEYSAVE  DS    CL25                                                             
KEY      DS    CL25                                                             
IO       DS    1000C                                                            
IO2      DS    1000C                                                            
*                                                                               
DDDSLST  DS    0C                                                               
         DSDDL                                                                  
WORKX    EQU   *                                                                
         EJECT                                                                  
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DSECT TO COVER TWA                                                            
*                                                                               
CTFUSFFD DSECT                                                                  
         DS    CL16                                                             
INITFLAG DS    X                   FIRST TIME PASS FLAG                         
*                                  1=NO USERID FIELD, 2=USERID FIELD            
MODE     DS    X                                                                
LSYSEQU  DS    C                                                                
LPRGNUM  DS    CL3                                                              
LFLDNUMS DS    XL3                                                              
LFILTERS DS    CL8                                                              
LAGYVALS DS    XL3                                                              
LINUNUM  DS    XL2                 LAST INPUT USER ID NUMBER                    
LKEY     DS    CL25                                                             
LACTION  DS    X                                                                
* CTFUSFFD                                                                      
       ++INCLUDE CTFUSFFSD                                                      
* ADDITIONAL SAVE AREA                                                          
FKEYFLAG DS    XL1                 DISPLAY FROM CHANGE SEQUENCE FLAG            
FKEYSAVE DS    CL25                FIRST KEY IN CURRENT DISPLAY                 
*                                                                               
CTPROFFD DSECT                                                                  
         DS    CL19                                                             
LKEY     DS    CL25                                                             
         DS    CL20                                                             
* CTPROFFD                                                                      
       ++INCLUDE CTPROFFSD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004CTFUS01   07/09/92'                                      
         END                                                                    
