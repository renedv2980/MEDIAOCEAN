*          DATA SET DDABEND    AT LEVEL 001 AS OF 10/14/19                      
*PHASE DDABENDA                                                                 
         TITLE 'JCL ABEND WITH NUMBER SUPPLIED AS PARM='                        
**********************************************************************          
* SIMPLE PROGRAM TO CAUSE ABEND AS PART OF A JCL STEP WITH SPECIFIC             
* ABEND CODE                                                                    
* PARM=304     - CAUSE ABEND304                                                 
**********************************************************************          
         PRINT NOGEN                                                            
DDABEND  CSECT                                                                  
         NBASE 0,ABEND,WORK=A(DDABEND)                                          
         ST    R1,ACMRG            SAVE MVS PARAM ADDRESS                       
         L     R1,0(R1)            ADDR OF PARM= DATA                           
         LH    R2,0(R1)            LENGTH OF PARM= DATA                         
         LTR   R2,R2               R2=L'PARM DATA                               
         JZ    *+2                 CAN'T BE ZERO                                
         AHI   R1,2                R1=A(PARM DATA)                              
         LR    R4,R1                                                            
         XR    RF,RF               COUNT LENGTH                                 
         CHI   RF,4                ABEND 0 < 4095                               
         JH    *+2                                                              
ABEND01  CLI   0(R1),C'0'                                                       
         JL    *+2                 HAS TO BE FROM 0 TO 9                        
         CLI   0(R1),C'9'                                                       
         JH    *+2                                                              
         AHI   R1,1                                                             
         AHI   RF,1                                                             
         JCT   R2,ABEND01                                                       
*                                                                               
         SHI   RF,1                                                             
         EXRL  RF,EXPACK                                                        
         NI    PACKED+L'PACKED-1,X'FC'                                          
         CVB   R3,PACKED                                                        
         CHI   R3,4096                                                          
         JNL   *+2                                                              
         ABEND (R3)                                                             
*                                                                               
ABEND99  XBASE                                                                  
*                                                                               
EXPACK   PACK  PACKED,0(0,R4)                                                   
***********************************************************************         
* TABLES AND DEFINITIONS                                                        
***********************************************************************         
         EJECT                                                                  
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
PACKED   DS    PL8                                                              
FULL     DS    F                                                                
ACMRG    DS    A                                                                
DMCB     DS    6F                                                               
PLIST    DS    6F                                                               
*                                                                               
P1       DC    F'0'                                                             
P2       DC    F'0'                                                             
P3       DC    F'0'                                                             
P4       DC    F'0'                                                             
P5       DC    F'0'                                                             
P6       DC    F'0'                                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDABEND   10/14/19'                                      
         END                                                                    
