*          DATA SET TZIHMAC    AT LEVEL 091 AS OF 10/18/00                      
*PHASE TZIHMAC                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
         TITLE 'TZIHMAC -- TESTING MACROS'                                      
* LEAVE THIS CODE ALONE                                                         
*                                                                               
TZIHMAC  CSECT                                                                  
                                                                                
***MY MACRO IS HERE***                                                          
                                                                                
         MACRO                                                                  
&LABEL   AS    &P1,&P2                                                          
                                                                                
.* INITIALIZE LOCAL VARIABLES                                                   
.* TYPES OF PARAMETERS                                                          
         LCLC  &T1,&T2                                                          
.* LENGTHS OF PARAMETERS                                                        
         LCLA  &LOP1,&LOP2                                                      
.* MASKS FOR PARAMETERS (TO BE USED WITH ICM, STCM)                             
         LCLA  &MASK1,&MASK2                                                    
.* LOOP COUNTER (TO BE USED IN MASK-CALCULATING LOOP)                           
         LCLA  &C                                                               
                                                                                
.* SET OPERAND LENGTHS                                                          
&LOP1    SETA  (L'&P1)                                                          
&LOP2    SETA  (L'&P2)                                                          
                                                                                
.* VALIDATE OPERAND LENGTHS AND SET MASK (FOR ICM, STCM)                        
         AIF   ((&LOP1 LT 1) OR (&LOP1 GT 4)).LERR2                             
         AIF   ((&LOP2 LT 1) OR (&LOP2 GT 4)).LERR2                             
         AIF   (&LOP1 LT &LOP2).LERR                                            
                                                                                
.* SET OPERAND TYPES                                                            
&T1      SETC  T'&P1                                                            
&T2      SETC  T'&P2                                                            
                                                                                
.* VALIDATE OPERAND TYPES                                                       
        AIF (('&T1' NE 'F') AND ('&T1' NE 'H') AND ('&T1' NE 'X')).TERR         
        AIF (('&T2' NE 'F') AND ('&T2' NE 'H') AND ('&T2' NE 'X')).TERR         
                                                                                
.* CALCULATE MASKS FOR ICM AND STCM INSTRUCTIONS                                
                                                                                
&C       SETA  &LOP1                                                            
&MASK1   SETA  1                                                                
.MLOOP1  ANOP                                                                   
         AIF   (&C LE 0).EMLOOP1                                                
&MASK1   SETA  &MASK1*2                                                         
&C       SETA  &C-1                                                             
         AGO   .MLOOP1                                                          
.EMLOOP1 ANOP                                                                   
&MASK1   SETA  &MASK1-1                                                         
                                                                                
&C       SETA  &LOP2                                                            
&MASK2   SETA  1                                                                
.MLOOP2  ANOP                                                                   
         AIF   (&C LE 0).EMLOOP2                                                
&MASK2   SETA  &MASK2*2                                                         
&C       SETA  &C-1                                                             
         AGO   .MLOOP2                                                          
.EMLOOP2 ANOP                                                                   
&MASK2   SETA  &MASK2-1                                                         
                                                                                
.***** CODE GENERATING PART *****                                               
                                                                                
.* THESE LINES ARE ALWAYS GENERATED                                             
         SR    R0,R0                                                            
         ICM   R0,&MASK1,&P1                                                    
                                                                                
         AIF   (T'&P2 NE 'F').HALF                                              
         A     R0,&P2                                                           
         AGO   .STOR                                                            
.HALF    ANOP                                                                   
         AIF   (T'&P2 NE 'H').OTHER                                             
         AH    R0,&P2                                                           
         AGO   .STOR                                                            
.OTHER   ANOP                                                                   
         SR    R1,R1                                                            
         ICM   R1,&MASK2,&P2                                                    
         AR    R0,R1                                                            
.STOR    ANOP                                                                   
.* THIS LINE IS ALWAYS GENERATED                                                
        STCM   R0,&MASK1,&P1                                                    
                                                                                
.*****END OF CODEGEN*****ERROR MESSAGES BELOW*****                              
                                                                                
.* EXIT STATEMENT TO SKIP ERROR MESSAGE                                         
         MEXIT                                                                  
                                                                                
.LERR    MNOTE 8,'***LENGTH OF OPERAND 1 IS LESS THAN OF OPERAND 2***'          
         MEXIT                                                                  
                                                                                
.LERR2   MNOTE 8,'***OPERAND LENGTH, L, HAS TO BE 1 <= L <= 4  ***'             
         MEXIT                                                                  
                                                                                
.TERR    MNOTE 8,'***ONLY BINARY OPERANDS ARE ALLOWED***'                       
         MEND                                                                   
                                                                                
***END OF MACRO***                                                              
                                                                                
         PRINT NOGEN                                                            
         NBASE 0,*TZIHMAC,=V(REGSAVE)                                           
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN10                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(TZIHMAC),V(DUMMY)                                              
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
MAIN10   DS    0H                                                               
         EJECT                                                                  
**********************                                                          
* * YOUR CODE HERE *                                                            
MAIN     DS    0H                                                               
                                                                                
ADDMAC1  AS    QBYTE1,FULL2                                                     
*                                                                               
ADDMAC1  AS    FULL1,QBYTE2                                                     
*                                                                               
ADDMAC1  AS    FULL1,TBYTE1                                                     
*                                                                               
ADDMAC1  AS    TBYTE1,HALF1                                                     
*                                                                               
ADDMAC1  AS    HALF1,DBYTE1                                                     
*                                                                               
ADDMAC1  AS    DBYTE1,BYTE1                                                     
*                                                                               
ADDMAC1  AS    BYTE1,BYTE2                                                      
*****FOLLOWING SHOULD ALL RESULT IN ERRORS                                      
ADDMAC1  AS    TBYTE1,FULL2                                                     
*                                                                               
ADDMAC1  AS    DUB,QBYTE2                                                       
*                                                                               
ADDMAC1  AS    CHAR,BYTE1                                                       
*                                                                               
ADDMAC1  AS    CHARL4,HALF1                                                     
*                                                                               
**********************                                                          
*                                                                               
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
                                                                                
DMCB     DS    6F                                                               
DUB      DS    D                                                                
FULL1    DS    F                                                                
FULL2    DS    F                                                                
HALF1    DS    H                                                                
HALF2    DS    H                                                                
BYTE1    DS    X                                                                
BYTE2    DS    X                                                                
DBYTE1   DS    XL2                                                              
DBYTE2   DS    XL2                                                              
TBYTE1   DS    XL3                                                              
TBYTE2   DS    XL3                                                              
QBYTE1   DS    XL4                                                              
QBYTE2   DS    XL4                                                              
CHAR     DS    C                                                                
CHARL4   DS    CL4                                                              
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
******************************************************                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'091TZIHMAC   10/18/00'                                      
         END                                                                    
