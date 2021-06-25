*          DATA SET IDFPGM     AT LEVEL 001 AS OF 12/07/99                      
*PHASE IDFPGM                                                                   
         TITLE 'TEST ASMIDF ON LOADED PRGRAM'                                   
         PRINT NOGEN                                                            
IDFPGM   CSECT                                                                  
         NMOD1 0,**IDFPGM                                                       
         L     RC,0(R1)            RC=A(COMMON STORAGE)                         
         USING WORKD,RC                                                         
         L     R4,ADCB             R4=A(SYSPRINT DCB)                           
         SPACE 2                                                                
         MVI   PLINE,C' '                                                       
         MVC   PLINE+1(L'PLINE-1),PLINE+0                                       
         MVC   PLINE+1(L'MESSAGE),MESSAGE                                       
         PUT   ((R4)),PLINE                                                     
         SPACE 2                                                                
         XMOD1 1                                                                
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
MESSAGE  DC    CL30'HELLO FROM LOADED PROGRAM'                                  
         SPACE 2                                                                
* WORKING STORAGE DSECT                                                         
*                                                                               
WORKD    DSECT                                                                  
DMCB     DS    6F                                                               
ADCB     DS    A                                                                
DATEOUT  DS    CL6                                                              
PLINE    DS    CL133                                                            
WORKL    EQU   *-WORKD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001IDFPGM    12/07/99'                                      
         END                                                                    
