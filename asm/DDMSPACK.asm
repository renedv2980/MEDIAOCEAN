*          DATA SET DDMSPACK   AT LEVEL 014 AS OF 10/21/14                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 044581.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE T00A1BA                                                                  
***********************************************************************         
*                                                                     *         
* MSPACK/MSUNPK HAVE BEEN EFFECTIVELY REPLACED BY STAPACK. ALL NEW    *         
* CODE SHOULD CALL STAPACK, NOT THIS MODULE.                          *         
*                                                                     *         
***********************************************************************         
         TITLE 'T00A1B - MARKET/STATION PACK'                                   
MSPACK   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 1,*MSPACK*                                                       
         USING MSPD,RC                                                          
         LR    R9,R1                                                            
         LM    R2,R4,0(R1)                                                      
         PACK  MSPDUB,0(4,R2)                                                   
         CVB   R1,MSPDUB                                                        
         STCM  R1,3,0(R4)                                                       
*                                                                               
         CLI   0(R3),C'0'          IS THIS A CABLE STATION                      
         BL    MSP30                                                            
         PACK  MSPDUB,0(4,R3)                                                   
         CVB   R1,MSPDUB                                                        
         SLL   R1,7                                                             
         CLC   5(3,R3),=C'   '     IS THERE A NETWORK                           
         BNH   MSP25                                                            
         LA    R5,CABLETAB                                                      
*                                                                               
MSP10    CLI   0(R5),X'FF'         END OF TABLE                                 
         BNE   MSP15                                                            
         MVI   0(R9),X'FF'         SET ERROR CODE                               
         B     MSP25               & SET PACKED STATION                         
*                                                                               
MSP15    ZIC   R6,4(R5)            LENGTH OF ENTRY                              
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),5(R3)       MATCH NETWORK CODE                           
         BE    MSP20                                                            
         LA    R5,L'CABLETAB(R5)                                                
         B     MSP10                                                            
*                                                                               
MSP20    ZIC   RE,3(R5)                                                         
         OR    R1,RE                                                            
*                                                                               
MSP25    STCM  R1,7,2(R4)                                                       
         OI    2(R4),X'F0'         SET ON HIGH BITS - CABLE                     
         B     MSPX                                                             
*                                                                               
MSP30    SR    R0,R0                                                            
         SR    R1,R1                                                            
         LA    R5,26                                                            
         LA    R8,2                                                             
         BAS   RE,MSPCHAR                                                       
         AR    R1,R6                                                            
         MR    R0,R5                                                            
         LA    R3,1(R3)                                                         
         BCT   R8,*-12                                                          
         BAS   RE,MSPCHAR                                                       
         AR    R1,R6                                                            
         LA    R5,27                                                            
         MR    R0,R5                                                            
         LA    R3,1(R3)                                                         
         BAS   RE,MSPCHAR                                                       
         AR    R1,R6                                                            
         SLL   R1,5                                                             
         STCM  R1,7,2(R4)          SAVE IN CASE OF MEDIA C ERROR                
         LA    R3,1(R3)                                                         
*                                                                               
         LA    R6,L'MSPMTAB                                                     
         LA    R7,MSPMTAB                                                       
         LR    R8,R7                                                            
         CLC   0(1,R3),0(R8)                                                    
         BE    *+16                                                             
         LA    R8,1(R8)                                                         
         BCT   R6,*-14                                                          
         B     MSPERR                                                           
         SR    R8,R7                                                            
         AR    R1,R8                                                            
         STCM  R1,7,2(R4)                                                       
         B     MSPX                                                             
*                                                                               
MSPCHAR  LR    R6,R5                                                            
         LA    R7,MSPCTAB                                                       
         CLC   0(1,R3),0(R7)                                                    
         BE    *+16                                                             
         LA    R7,1(R7)                                                         
         BCT   R6,*-14                                                          
         B     MSPERR                                                           
         SR    R6,R5                                                            
         LPR   R6,R6                                                            
         BR    RE                                                               
*                                                                               
MSPERR   MVI   0(R9),X'FF'                                                      
*                                                                               
MSPX     XMOD1 1                                                                
         SPACE 2                                                                
MSPCTAB  DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ '                                   
MSPMTAB  DC    CL5'AFTNX'                                                       
         SPACE 2                                                                
       ++INCLUDE SPCBLLST                                                       
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 1                                                                
MSPD     DSECT                                                                  
MSPDUB   DS    PL8                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014DDMSPACK  10/21/14'                                      
         END                                                                    
