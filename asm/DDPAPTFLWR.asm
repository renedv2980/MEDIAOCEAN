*          DATA SET DDPAPTFLWR AT LEVEL 002 AS OF 07/13/09                      
*PHASE PAPTFLRA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PANIC                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'PANAPT: ADD FLOWER BOX TO SRCE LIBCODE MEMBERS'                 
PAPTFLR  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,PAPTFLR,=V(REGSAVE)                                            
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         LR    R1,RC                                                            
         SHI   R1,4                                                             
         L     R1,0(R1)                                                         
         L     R1,0(R1)            A(PARM= FROM EXEC JCL CARD)                  
         CLC   =H'9',0(R1)         PARM LENGTH                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'MR=',2(R1)       MR# MUST BE PROVIDED                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MR#,5(R1)           SAVE MR#                                     
*                                                                               
         OPEN  (SYSPUNCH,OUTPUT)                                                
*                                                                               
GETDIR   DS    0H                                                               
*                                  GET THE NAME RECORD                          
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    CLOSE                                                            
*                                                                               
         GOTO1 =V(PANIC),DMCB,(0,=C'READ'),=C'DIR',CARD,DIRECTRY                
         CLI   DMCB+8,0            PAN BOOK FOUND?                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,DIRECTRY         0-UP DIRECTORY ENTRY                         
         USING DIRENTRY,R4                                                      
*                                                                               
         MVC   OUTREC,SPACES       LEVEL THE MEMBER BACK BY 1                   
         MVC   OUTREC(8),=C'++LEVEL '                                           
         MVC   OUTREC+8(10),DNAME  MEMBER NAME                                  
         LA    RF,OUTREC+8+L'DNAME-1                                            
         CLI   0(RF),C' '          POINT RF TO LAST CHAR IN MEMBER NAME         
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
         MVI   1(RF),C','                                                       
         MVC   2(3,RF),DLEVEL      CURRENT LEVEL NUMBER                         
         MVI   5(RF),C','                                                       
         PACK  DUB,DLEVEL                                                       
         CVB   R0,DUB                                                           
         CHI   R0,1                LEVEL 1?                                     
         BNE   *+8                                                              
         LHI   R0,256              YES: FORCE ROLLBACK TO LEVEL 255             
         BCTR  R0,0                DECREMENT THE LEVEL NUMBER                   
         CVD   R0,DUB                                                           
         UNPK  6(3,RF),DUB+6(2)                                                 
         OI    8(RF),X'F0'                                                      
         MVC   NEXTLVL,6(RF)       SAVE THE NEW LEVEL NUMBER                    
         PUT   SYSPUNCH,OUTREC                                                  
         MVC   P(80),OUTREC                                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   OUTREC,SPACES       ++UPDATE FOR FLOWER BOX                      
         MVC   OUTREC(9),=C'++UPDATE '                                          
         MVC   OUTREC+9(10),DNAME  MEMBER NAME                                  
         LA    RF,OUTREC+9+L'DNAME-1                                            
         CLI   0(RF),C' '          POINT RF TO LAST CHAR IN MEMBER NAME         
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
         MVI   1(RF),C','                                                       
         MVC   2(3,RF),NEXTLVL     CURRENT LEVEL NUMBER                         
         PUT   SYSPUNCH,OUTREC                                                  
         MVC   P(80),OUTREC                                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   OUTREC,SPACES       INSERT FLOWERBOX AT BEGINNING OF MBR         
         MVC   OUTREC(5),=C'++C 0'                                              
         PUT   SYSPUNCH,OUTREC                                                  
         MVC   P(80),OUTREC                                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R5,FLOWERBOX        GENERATE THE FLOWER BOX                      
PUTFLOWR PUT   SYSPUNCH,(5)                                                     
         MVC   P(80),0(R5)                                                      
         GOTO1 =V(PRINTER)                                                      
         LA    R5,80(R5)                                                        
         CLI   0(R5),X'FF'                                                      
         BNE   PUTFLOWR                                                         
*                                                                               
         GOTO1 =V(PANIC),DMCB,=C'CLOSE',=C'PAN'                                 
         DROP  R4                                                               
*                                                                               
         B     GETDIR              NEXT SRCE MEMBER                             
*                                                                               
CLOSE    DS    0H                                                               
         CLOSE (SYSPUNCH)                                                       
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
NEXTLVL  DS    CL3                 BUMPED UP LEVEL NUMBER                       
*                                                                               
CARD     DS    CL80                SYSIN INPUT (MEMBER NAME)                    
DIRECTRY DS    CL80                PAN MEMBER DIRECTORY RECORD (0-UP)           
OUTREC   DS    CL80                OUTPUT RECORD                                
         SPACE 3                                                                
* NOTE: THERE IS A HARD COMPARE IN DDPAPTMRM FOR THE $PANAPT01S$ TAG!           
*                                                                               
FLOWERBOX DS 0D                                                                 
 DC CL80'*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************'         
 DC CL80'*                                                           *'         
 DC CL80'*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *'         
 DC CL80'*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *'         
 DC CL80'*                                                           *'         
 DC CL80'*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *'         
 DC CL80'*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *'         
 DC C'*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# '                          
MR# DS CL6                                                                      
 DC CL27'.      *'                                                              
 DC CL80'*                                                           *'         
 DC CL80'*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *'         
 DC CL80'*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *'         
 DC CL80'*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *'         
 DC CL80'*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *'         
 DC CL80'*                                                           *'         
 DC CL80'*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************'         
 DC X'FF'                                                                       
*                                                                               
SYSPUNCH DCB   DDNAME=SYSPUNCH,DSORG=PS,LRECL=80,RECFM=FB,MACRF=PM              
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 3                                                                
* ++INCLUDE DDPAN0UPD                                                           
* ++INCLUDE DDDPRINT                                                            
         PRINT OFF                                                              
       ++INCLUDE DDPAN0UPD                                                      
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DDPAPTFLWR07/13/09'                                      
         END                                                                    
