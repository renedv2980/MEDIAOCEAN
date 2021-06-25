*          DATA SET DDMSUNPK   AT LEVEL 009 AS OF 10/21/14                      
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
*PHASE T00A1CA                                                                  
***********************************************************************         
*                                                                     *         
* MSPACK/MSUNPK HAVE BEEN EFFECTIVELY REPLACED BY STAPACK. ALL NEW    *         
* CODE SHOULD CALL STAPACK, NOT THIS MODULE.                          *         
*                                                                     *         
***********************************************************************         
         TITLE 'T00A1C - MARKET/STATION UNPACK'                                 
MSUNPK   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 1,*MSUNPK*                                                       
         USING MSUD,RC                                                          
         LM    R2,R4,0(R1)                                                      
         TM    0(R1),X'80'         NEW 8 CHAR FIELD                             
         BZ    *+10                                                             
         MVC   5(3,R4),=C'   '                                                  
         SR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         CVD   RE,MSUDUB                                                        
         OI    MSUDUB+7,X'0F'                                                   
         UNPK  0(4,R3),MSUDUB                                                   
         L     R9,2(R2)                                                         
         TM    2(R2),X'F0'         IF X'F0' IS ON - IT BE CABLE                 
         BNO   MSUNPK30                                                         
         SLL   R9,4                IGNORE X'F0'                                 
         SRL   R9,19                                                            
         CVD   R9,MSUDUB                                                        
         OI    MSUDUB+7,X'0F'                                                   
         UNPK  0(4,R4),MSUDUB                                                   
         MVI   4(R4),C' '                                                       
         TM    0(R1),X'80'         DO I HAVE 8 CHAR OUTPUT                      
         BZ    MSUNPKX                                                          
         L     R9,2(R2)                                                         
         SLL   R9,17                                                            
         SRL   R9,25                                                            
         STC   R9,MSUDUB                                                        
         LA    R5,CABLETAB                                                      
*                                                                               
MSUNPK10 CLI   0(R5),X'FF'                                                      
         BE    MSUNPKX                                                          
         CLC   3(1,R5),MSUDUB                                                   
         BE    MSUNPK20                                                         
         LA    R5,L'CABLETAB(R5)                                                
         B     MSUNPK10                                                         
*                                                                               
MSUNPK20 MVC   5(3,R4),0(R5)                                                    
         B     MSUNPKX                                                          
*                                                                               
MSUNPK30 SRL   R9,13                                                            
         LA    R5,3(R4)                                                         
         LA    R6,27                                                            
         SR    R8,R8                                                            
         DR    R8,R6                                                            
         LA    R8,MSUCTAB(R8)                                                   
         MVC   0(1,R5),0(R8)                                                    
         BCTR  R5,0                                                             
         BCTR  R6,0                                                             
         LA    R7,2                                                             
*                                                                               
MSUNPK40 SR    R8,R8                                                            
         DR    R8,R6                                                            
         LA    R8,MSUCTAB(R8)                                                   
         MVC   0(1,R5),0(R8)                                                    
         BCTR  R5,0                                                             
         BCT   R7,MSUNPK40                                                      
         LA    R9,MSUCTAB(R9)                                                   
         MVC   0(1,R5),0(R9)                                                    
         ZIC   R8,4(R2)                                                         
         SLL   R8,27                                                            
         SRL   R8,27                                                            
         MVI   4(R4),C'N'                                                       
         CH    R8,=H'5'                                                         
         BNL   MSUNPKX                                                          
         LA    R8,MSUMTAB(R8)                                                   
         MVC   4(1,R4),0(R8)                                                    
*                                                                               
MSUNPKX  XMOD1 1                                                                
         SPACE 2                                                                
MSUCTAB  DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ '                                   
MSUMTAB  DC    C'AF NXNNNNNNNNNNN'                                              
*                0123456789ABCDEF                                               
         LTORG                                                                  
         SPACE 2                                                                
       ++INCLUDE SPCBLLST                                                       
         SPACE 2                                                                
MSUD     DSECT                                                                  
MSUDUB   DS    PL8                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009DDMSUNPK  10/21/14'                                      
         END                                                                    
