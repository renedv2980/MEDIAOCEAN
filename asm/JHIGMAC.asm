*          DATA SET JHIGMAC    AT LEVEL 255 AS OF 10/18/00                      
*PHASE JHIGMAC                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
         TITLE 'JHIGDATE -- DATE CONVERSION'                                    
***********************************************************************         
         EJECT                                                                  
* LEAVE THIS CODE ALONE                                                         
*                                                                               
DATECONV CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*DATECONV,=V(REGSAVE)                                          
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN10                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(DATECONV),V(DUMMY)                                             
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
MAIN10   DS    0H                                                               
         EJECT                                                                  
**********************                                                          
* PUT YOUR CODE HERE *                                                          
**********************                                                          
*                                                                               
********************                                                            
* MACRO DEFINITION *                                                            
********************                                                            
         MACRO                                                                  
&LABEL   AS    &F1,&F2                                                          
         LCLA  &MASK1,&MASK2                                                    
                                                                                
         AIF   (L'&F2 GT L'&F1).ERR                                             
         AIF   (L'&F1 EQ 4).FOUR                                                
         AIF   (L'F1 EQ 3).THREE                                                
         AIF   (L'&F1 EQ 2).TWO                                                 
         AIF   (L'&F1 EQ 1).ONE                                                 
         MNOTE 8,'**PARAMETER ONE EXCEEDS MAX LENGTH**'                         
         MEXIT                                                                  
.FOUR    DS    0H                                                               
&MASK1   SETA  15                                                               
         AGO   .MASK2                                                           
.THREE   DS    0H                                                               
&MASK1   SETA  7                                                                
         AGO   .MASK2                                                           
.TWO     DS    0H                                                               
&MASK1   SETA  3                                                                
         AGO   .MASK2                                                           
.ONE     DS    0H                                                               
&MASK1   SETA  1                                                                
.*                                                                              
.MASK2   AIF   (L'&F2 EQ 4).FOURX                                               
         AIF   (L'&F2 EQ 3).THREEX                                              
         AIF   (L'&F2 EQ 2).TWOX                                                
         AIF   (L'&F2 EQ 1).ONEX                                                
.FOURX   DS    0H                                                               
&MASK2   SETA  15                                                               
         AGO   .COMP                                                            
.*                                                                              
.THREEX  DS    0H                                                               
&MASK2   SETA  7                                                                
         AGO   .COMP                                                            
.*                                                                              
.TWOX    DS    0H                                                               
&MASK2   SETA  3                                                                
         AGO   .COMP                                                            
.*                                                                              
.ONEX    DS    0H                                                               
&MASK2   SETA  1                                                                
.*                                                                              
.COMP    SR    R0,R0                                                            
         ICM   R0,&MASK1,&F1                                                    
         SR    R1,R1                                                            
         ICM   R1,&MASK2,&F2                                                    
         AR    R0,R1                                                            
         STCM  R0,&MASK1,&F1                                                    
         AGO   .E                                                               
*                                                                               
.ERR     MNOTE 8,'**ATTEMPT TO ADD LARGE TO SMALL IN AS INSTRUCTION**'          
.E       MEND                                                                   
*************                                                                   
* END MACRO *                                                                   
*************                                                                   
         LA    R1,18                                                            
         LA    R2,22                                                            
         STCM  R1,15,BYTE                                                       
         STCM  R2,3,BYTE2                                                       
         AS    BYTE,BYTE2                                                       
         EDIT  BYTE,(10,P)                                                      
         GOTO1 =V(PRINTER)                                                      
         XBASE                                                                  
*****************************************                                       
*                                                                               
         EJECT                                                                  
DUB      DS    D                                                                
OUTP     DS    D                                                                
DMCB     DS    6F                                                               
FULL     DS    F                                                                
FULL1    DS    F                                                                
FULL2    DS    F                                                                
Y1STORE  DS    F                                                                
Y2STORE  DS    F                                                                
D1STORE  DS    F                                                                
D2STORE  DS    F                                                                
DAY1ANS  DS    F                                                                
DAY2ANS  DS    F                                                                
DAYTOT   DS    F                                                                
TESTLEAP DS    F                                                                
HALF1    DS    H                                                                
HALF2    DS    H                                                                
LEAPFLAG DS    X                                                                
SIGNFLAG DS    X                                                                
SAMEYR   DS    X                                                                
BYTE     DS    XL4                                                              
BYTE2    DS    XL2                                                              
MONTH1   DS    CL3                                                              
MONTH2   DS    CL3                                                              
IOAREA   DS    CL80                                                             
WORK     DS    CL64                                                             
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
******************************************************                          
         SPACE 3                                                                
         LTORG                                                                  
*                                                                               
DAYMON   DSECT                                                                  
DAYD     DS    H                                                                
MONTHD   DS    CL3                                                              
DAYSD    DS    H                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'255JHIGMAC   10/18/00'                                      
         END                                                                    
