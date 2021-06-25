*          DATA SET DDXSORTS   AT LEVEL 009 AS OF 08/13/00                      
*PHASE T00A12A                                                                  
         TITLE 'XSORT - EXCHANGE SORT MODULE'                                   
         SPACE 3                                                                
*        PARAMETER LIST                                                         
         SPACE 1                                                                
*        PARAM1  BYTE  0      X'00' = ASCENDING,X'FF' = DESCENDING              
*                BYTES 1-3    A(LIST)                                           
         SPACE 1                                                                
*        PARAM2  BYTES 0-3    NO. OF ENTRIES                                    
         SPACE 1                                                                
*        PARAM3  BYTE    3    LENGTH OF ENTRY                                   
         SPACE 1                                                                
*        PARAM4  BYTE    3    LENGTH OF KEY                                     
         SPACE 1                                                                
*        PARAM5  BYTE    3    DISPLACEMENT OF KEY INTO ENTRY                    
         SPACE 3                                                                
XSORT    CSECT                                                                  
         NMOD1 0,XSORT                                                          
         EJECT                                                                  
*                  PROCESS PARAMETER LIST                                       
         SPACE 3                                                                
         L     R3,4(R1)            NO. OF ENTRIES                               
         BCTR  R3,R0                                                            
         LTR   R3,R3                                                            
         BNP   XSORTEXT            FEWER THAN 2 ENTRIES                         
         L     R4,8(R1)            R4 = LENGTH OF ENTRY                         
         MR    R2,R4                                                            
         LR    R5,R3               LENGTH * (NO. OF ENTRIES-1)                  
         L     R3,0(1)             R3 = A(FIRST ENTRY)                          
         LA    R3,0(R3)           CLEAR HOB                                     
         AR    R5,R3                                                            
         BCTR  R5,R0               R5 = A((LAST ENTRY) - 1)                     
         LCR   R2,R4               R2 = COMPLEMENT OF LENGTH                    
         MVI   XSORTBC+1,X'D0'     SET BRANCH TO BNH IF ACSENDING               
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         MVI   XSORTBC+1,X'B0'     SET BRANCH TO BNL IF DESCENDING              
         LR    R6,R4                                                            
         AH    R6,=X'3000'         NOTE - R3 USED IN XC'S                       
*                                  DISPLACEMENTS IN XC'S                        
         STH   R6,XSORTXC1+4                                                    
         STH   R6,XSORTXC2+2                                                    
         STH   R6,XSORTXC3+4                                                    
         BCTR  R6,R0                                                            
*                                  LENGTHS IN XC'S                              
         STC   R6,XSORTXC1+1                                                    
         STC   R6,XSORTXC2+1                                                    
         STC   R6,XSORTXC3+1                                                    
         L     R7,12(R1)                                                        
         BCTR  R7,R0                                                            
*                                  LENGTH IN CLC                                
         STC   R7,XSORTCLC+1                                                    
         L     R7,16(R1)                                                        
*                                  DISPLACEMENTS IN CLC                         
         STC   R7,XSORTCLC+3                                                    
         LA    R7,1(R7,R6)         ADD ENTRY LENGTH                             
         STH   R7,XSORTCLC+4                                                    
*                                                                               
         SR    R6,R6                                                            
         LR    R7,R3               SAVE START OF LIST                           
         EJECT                                                                  
*                   SORT LOGIC                                                  
         SPACE 3                                                                
XSORTCLC CLC   0(0,R3),0(R3)       COMPARE KEYS                                 
XSORTBC  BC    0,XSORTNOX          NO EXCHANGE                                  
XSORTXC1 XC    0(0,R3),0(R3)       EXCHANGE                                     
XSORTXC2 XC    0(0,R3),0(R3)        THE                                         
XSORTXC3 XC    0(0,R3),0(R3)         ENTRIES                                    
         LA    R6,1                RESET SWITCH                                 
XSORTNOX BXLE  R3,R4,XSORTCLC      NEXT COMPARE                                 
         BCT   R6,XSORTEXT         EXIT IF NO EXCHANGES + RESET SW              
         LR    R3,R7               RESTORE TO START OF LIST                     
         BXH   R5,R2,XSORTCLC      SHORTEN LIST AND DO ANOTHER PASS             
XSORTEXT XMOD1 1                                                                
         SPACE 2                                                                
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009DDXSORTS  08/13/00'                                      
         END                                                                    
